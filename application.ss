#lang scheme/gui

(require "utils.ss"
         "components.ss"
         srfi/43
         mred
         framework
         scheme/class
         scheme/match)

(provide original% parody% application%)

(define (load-file filename)
  ; read in file into two parallel vectors
  (let [(input-file (open-input-file filename))]
    (let-values ([(read-o read-p _)
                  (for/fold ([o '()] [p '()] [c "# "])
                    ([line (in-lines input-file)])
                    (match (list (substring line 0 2) (substring line 2))
                      [(list "# " rest)
                       (cond [(equal? c "# ") (values (append-to-car rest o) p "# ")]
                             [else            (values (cons rest o) p "# ")])]
                      [(list "= " rest)
                       (cond [(equal? c "= ") (values o (append-to-car rest p) "= ")]
                             [else            (values o (cons rest p) "= ")])]))])
      (close-input-port input-file)
      (list (list->vector (reverse read-o)) (list->vector (reverse read-p))))))

(define original%
  (class object%
    (init-field parent)
    (init-field position)
    (init-field (text ""))
    
    (define editor (new read-only-text% (initial-text text)))
    (define snip (instantiate editor-snip% (editor) (min-width 791) (with-border? #f)))
    (send snip use-style-background #t)
    (send snip set-style (send the-style-list find-named-style "Original"))
    (define/public (get-editor) editor)
    (define/public (get-snip) snip)))

(define parody%
  (class object%
    (init-field parent)
    (init-field position)
    (init-field (text ""))
    (define editor (new editable-text% 
                        (initial-text text)
                        (on-height-changed (λ () (send parent height-changed position)))
                        (next-editor (λ () (send parent next-editor position)))
                        (prev-editor (λ () (send parent prev-editor position)))
                        (set-active (λ () (send parent set-active-editor position)))))
    (define snip (instantiate editor-snip% (editor) (min-width 790) (with-border? #f)))
    (define/public (get-editor) editor)
    (define/public (get-snip) snip)))

(define application%
  (class object%
    (define originals #f)
    (define parodies #f)
    (define n-lines 0)
    (define frame (instantiate frame% ("Dandelion" #f 800 600)))
    (define canvas (instantiate editor-canvas% (frame)))
    (define pasteboard (instantiate pasteboard% ()))
    (define current-line 1)
    (define (current-editor)
      (send (vector-ref parodies current-line) get-editor))
    
    
    
    (define (set-active-line i)
      (when (<> current-line i)
        (send (current-editor) show-border #f)
        (set! current-line (cond [(< i 0) 0] [(> i (- n-lines 1)) (- n-lines 1)] [else i]))
        (let ((ed (current-editor)))
          (send pasteboard set-caret-owner ed 'global)
          (send ed show-border #t)
          (send pasteboard scroll-to ed 0 -360 0 400 #t 'none))))
    
    (define (make-original text line)
      (new original% 
        (parent this)
        (position line)
        (text text)))
    
    (define (make-parody text line)
      (new parody% 
        (parent this)
        (position line)
        (text text)))
    
    (super-new)
    (send canvas set-editor pasteboard)
    (send pasteboard set-dragable #f)))