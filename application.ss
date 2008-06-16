#lang scheme/gui

(require "utils.ss"
         "components.ss"
         srfi/43
         mred
         framework
         scheme/class
         scheme/match)

(provide original% parody% application%)

(define editor-and-snip%
  (class object%
    (super-new)
    
    (define/public (get-editor) #f)
    (define/public (get-snip) #f)
    (define/public (get-height)
      (let ((h (box 0)))
        (send (get-snip) get-extent (send (get-editor) get-dc) 1.0 10.0 #f h)
        (unbox h)))))

(define original%
  (class editor-and-snip%
    (init-field parent)
    (init-field position)
    (init-field text)
    
    (define editor (new read-only-text% (initial-text text)))
    (define snip (instantiate editor-snip% (editor) (min-width 791) (with-border? #f)))
    (send snip use-style-background #t)
    (send snip set-style (send the-style-list find-named-style "Original"))
    (define/override (get-editor) editor)
    (define/override (get-snip) snip)
    (define/public (save-text) (prefix-lines "# " (send editor get-text)))
    
    (super-new)))

(define parody%
  (class editor-and-snip%
    (init-field parent)
    (init-field position)
    (init-field text)
    
    (define editor (new editable-text% 
                        (initial-text text)
                        (on-height-changed (λ () (send parent height-changed position)))
                        (next-editor (λ () (send parent next-editor position)))
                        (prev-editor (λ () (send parent prev-editor position)))
                        (set-active (λ () (send parent set-active-editor position)))))
    
    (define snip (instantiate editor-snip% (editor) (min-width 790) (with-border? #f)))
    (define/override (get-editor) editor)
    (define/override (get-snip) snip)
    (define/public (save-text) (prefix-lines "= " (send editor get-text)))
    
    (super-new)))

(define application%
  (class object%
    (define originals #())
    (define parodies #())
    
    (define n-lines 0)
    (define frame (instantiate frame% ("Dandelion" #f 800 600)))
    (define canvas (instantiate editor-canvas% (frame)))
    (define pasteboard (instantiate pasteboard% ()))
    (define current-line 1)
    (define current-file #f)
    
    (define/public (get-originals) originals)
    (define/public (get-parodies) parodies)
    
    (define (load-file-prompt i e) 
      (let ((a (finder:get-file #f "Select file" (byte-regexp #".*.dnd") "Not a .dnd file")))
        (when a (load-file a))))
    
    (define (save-file-prompt i e) 
      (let ((a (finder:put-file)))
        (when a (save-file a))))
    
    (define (save-current-file i e) 
      (if current-file 
          (save-file current-file)
          (save-file-prompt #f #f)))
          
    (define (import-file-prompt i e) #f)
    
    ; top menu
    (define mb (instantiate menu-bar% (frame)))
    (define m-file (instantiate menu% ("&File" mb)))
    (define file/open    (instantiate menu-item% ("&Open"      m-file load-file-prompt)))
    (define file/save    (instantiate menu-item% ("&Save"      m-file save-current-file)))
    (define file/save-as (instantiate menu-item% ("Save &As"   m-file save-file-prompt)))
    (define file/import  (instantiate menu-item% ("&Import"    m-file import-file-prompt)))
    (define file/quit    (instantiate menu-item% ("&Quit"      m-file (λ (i e) (exit:exit)))))
    
    (define/public (current-editor)
      (send (vector-ref parodies current-line) get-snip))
    
    (define/public (next-editor i)
      (set-active-line (+ current-line 1)))
    
    (define/public (prev-editor i)
      (set-active-line (- current-line 1)))
    
    (define/public (height-changed i) (update-all))
    
    (define/public (set-active-editor i) (set-active-line i))
    
    (define (set-active-line i)
      (when (<> current-line i)
        (send (current-editor) show-border #f)
        (set! current-line (cond [(< i 0) 0] [(> i (- n-lines 1)) (- n-lines 1)] [else i]))
        (let ((ed (current-editor)))
          (send pasteboard set-caret-owner ed 'global)
          (send ed show-border #t)
          (send pasteboard scroll-to ed 0 -360 0 400 #t 'none))))
    
    (define (make-original text line)
      (new original% (text text) (parent this) (position line)))
    
    (define (make-parody text line)
      (new parody% (parent this) (position line) (text text)))
    
    (define/public (load-file filename)
      (send pasteboard begin-edit-sequence)
      (delete-all)
      (match-let ([(list origs pars) (file->vectors filename)])
        (set! n-lines (vector-length origs))
        (set! originals (make-vector n-lines))
        (set! parodies  (make-vector n-lines))
        (for ([i (in-naturals)] [o (in-vector origs)] [p (in-vector pars)])
          (let ((orig (make-original o i))
                (par (make-parody p i)))
            (vector-set! originals i orig)
            (vector-set! parodies i par)
            (send pasteboard insert (send orig get-snip))
            (send pasteboard insert (send par get-snip))))
        (update-all)
        (set-active-line 0)
        (send pasteboard end-edit-sequence)
        (set! current-file filename)
        ))
    
    (define/public (save-file filename)
      (call-with-output-file filename #:exists 'truncate
        (λ (out)
          (for ([o (in-vector originals)] [p (in-vector parodies)])
            (display (send o save-text) out)
            (newline out)
            (display (send p save-text) out)
            (newline out)))))
    
    (define (delete-all)
      (for ([o (in-vector originals)] [p (in-vector parodies)])
        (send pasteboard delete (send o get-snip))
        (send pasteboard delete (send p get-snip))))
    
    (define (update-all)
      (let ((y 0.0))
        (vector-for-each
         (lambda (i orig par)
           (send pasteboard move-to (send orig get-snip) 0 y)
           (set! y (+ y (send orig get-height)))
           (send pasteboard move-to (send par get-snip) 0 y)
           (set! y (+ y (send par get-height))))
         originals parodies)))
    
    (define/public (show) (send frame show #t))
    
    (super-new)
    (send canvas set-editor pasteboard)
    (send pasteboard set-dragable #f)
    ))