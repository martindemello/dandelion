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
  (define (line-vec) (make-vector (vector-length lines)))
  
  (define origs (line-vec))
  (define mines (line-vec))
  (define texts (line-vec))
  (define edits (line-vec))
  
  ; create editors for original and new lines
  (vector-for-each
   (lambda (i line)
     (vector-set! origs i (new read-only-text% (initial-text line)))
     (vector-set! mines i (instantiate text% ())))
   lines)
  
  (vector-for-each
   (lambda (i orig mine)
     (vector-set! texts i (instantiate editor-snip% (orig) (with-border? #f)))
     (vector-set! edits i (instantiate editor-snip% (mine) (min-width 500) (with-border? #f))))
   origs mines)
  
  ; insert editors into the canvas
  (vector-for-each 
   (lambda (i text edit) 
     (send* p 
       (insert text)
       (insert edit)))
   texts edits)
  
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
  
  (calculate-positions)
  (update-positions)
  
  (send f show #t)
  )