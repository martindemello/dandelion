;;; Here’s the basic problem: you’re writing a text editor. Stop doing that. It’s 2007. --  Mark Pilgrim

(module dandelion mzscheme
  
  (require "utils.ss"
           "components.ss"
           (lib "43.ss" "srfi")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss"))
  
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
  
  (define lines (list->vector (readlines "revenge.txt")))
  
  (define (map-lines f) (vector-map f lines))
  
  (define origs 
    (map-lines
     (lambda (i line) 
       (let ((o (instantiate read-only-text% ())))
         (send o insert-text line)
         o))))
  
  (define mines 
    (map-lines (lambda (i line) (instantiate text% ()))))
  
  (define texts
    (map-lines 
     (lambda (i line) 
       (let ((o (vector-ref origs i)))
         (instantiate editor-snip% (o) (with-border? #f))))))
  
  (define edits 
    (map-lines 
     (lambda (i line) 
       (let ((m (vector-ref mines i)))
         (instantiate editor-snip% (m) (min-width 500) (with-border? #f))))))
  
  (define hres 24)
  (define y 0)
  (vector-for-each 
   (lambda (i line orig mine text edit) 
     (if (not (equal? line ""))
         (begin
           (send* p 
             (insert text)
             (move-to text 0 (* y hres))
             (insert edit))
           (set! y (+ y 1))
           (send p move-to edit 0 (* y hres))))
     (set! y (+ y 1)))
   lines origs mines texts edits)
  
  (send f show #t)
  )