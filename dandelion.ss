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
  
  (define (map-lines f) (vector-map f lines))
  
  (define origs 
    (map-lines (lambda (i line) (new read-only-text% (initial-text line)))))
  
  (define mines 
    (map-lines (lambda (i line) (instantiate text% ()))))
  
  (define texts
    (vector-map 
     (lambda (i orig) 
       (instantiate editor-snip% (orig) (with-border? #f)))
     origs))
  
  (define edits 
    (map-lines 
     (lambda (i line) 
       (let ((m (vector-ref mines i)))
         (instantiate editor-snip% (m) (min-width 500) (with-border? #f))))))
  
  ; insert editors into the canvas
  (vector-for-each 
   (lambda (i text edit) 
     (send* p 
       (insert text)
       (insert edit)))
   texts edits)
  
  ; positioning
  (define hres 24)
  
  (define yts
    (map-lines (lambda (i line) (* 2 i hres))))
  
  (define yes
    (map-lines (lambda (i line) (* (+ (* 2 i) 1) hres))))
  
  (define (update-positions)
      (vector-for-each
       (lambda (i text edit yt ye)
         (send* p
           (move-to text 0 yt)
           (move-to edit 0 ye)))
       texts edits yts yes))
  
  (update-positions)
  
  (send f show #t)
  )