;;; Here’s the basic problem: you’re writing a text editor. Stop doing that. It’s 2007. --  Mark Pilgrim

(module dandelion mzscheme
  
  (require "utils.ss"
           "components.ss"
           (lib "43.ss" "srfi")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss"))
  
  ; app gui
  (define f (instantiate frame% ("Dandelion" #f 800 600)))
  (define c (instantiate editor-canvas% (f)))
  (define p (instantiate pasteboard% ()))
  (define mb (instantiate menu-bar% (f)))
  (define m-edit (instantiate menu% ("Edit" mb)))
  (define m-font (instantiate menu% ("Font" mb)))
  (append-editor-operation-menu-items m-edit #f)
  (append-editor-font-menu-items m-font)
  (send c set-editor p)
  (send p set-dragable #f)
  
  ; create editors for each line in the input file
  (define lines (list->vector (readlines "revenge.txt")))
  (define n-lines (vector-length lines))
  (define (line-vec) (make-vector n-lines))
  
  (define origs (line-vec))
  (define mines (line-vec))
  (define texts (line-vec))
  (define edits (line-vec))
  
  (define (edit. n) (vector-ref edits n))
  (define (mine. n) (vector-ref mines n))
  
  ; positioning
  (define hres 24)
  
  (define yts (line-vec))
  (define yes (line-vec))
  
  (define (calculate-positions)
    (let ((y 0))
      (vector-for-each
       (lambda (i orig mine)
         (vector-set! yts i (* y hres))
         (set! y (+ y 1 (send orig last-line)))
         (vector-set! yes i (* y hres))
         (set! y (+ y 1 (send mine last-line))))
       origs mines)))
  
  (define (update-positions)
    (vector-for-each
     (lambda (i text edit yt ye)
       (send* p
         (move-to text 0 yt)
         (move-to edit 0 ye)))
     texts edits yts yes))
  
  (define (update-all)
    (calculate-positions)
    (update-positions))
  
  (define current-edit 1)
  
  (define (set-active-edit i)
    (cond [(<> current-edit i)
           (send (edit. current-edit) show-border #f)
           (set! current-edit (cond [(< i 0) 0] [(> i (- n-lines 1)) (- n-lines 1)] [else i]))
           (let ((ed (edit. current-edit)))
             (send p set-caret-owner ed 'global)
             (send ed show-border #t)
             (send p scroll-to ed 0 -360 0 400 #t 'none))
           ]
          ))
  
  (define (next-edit)
    (set-active-edit (+ 1 current-edit)))
  
  (define (prev-edit)
    (set-active-edit (- current-edit 1)))
  
  ; create editors for original and new lines
  (vector-for-each
   (lambda (i line)
     (vector-set! origs i (new read-only-text% (initial-text line)))
     (vector-set! mines i (new editable-text% 
                               (on-height-changed update-all)
                               (next-editor next-edit)
                               (prev-editor prev-edit)
                               (set-active (lambda () (set-active-edit i)))
                               )))
   lines)
  
  (vector-for-each
   (lambda (i orig mine)
     (let ((text (instantiate editor-snip% (orig) (min-width 791) (with-border? #f))))
       (vector-set! texts i text)
       (send text use-style-background #t)
       (send text set-style (send the-style-list find-named-style "Original")))
     (vector-set! edits i (instantiate editor-snip% (mine) (min-width 790) (with-border? #f))))
   origs mines)
  
  ; insert editors into the canvas
  (vector-for-each
   (lambda (i text edit)
     (send* p
       (insert text)
       (insert edit)))
   texts edits)
  
  (update-all)
  
  ; give the first editor the keyboard focus and caret
  (set-active-edit 0)
  
  (send f show #t)
  
  )