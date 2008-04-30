;;; Here’s the basic problem: you’re writing a text editor. Stop doing that. It’s 2007. --  Mark Pilgrim

(module dandelion mzscheme
  
  (require "utils.ss"
           "components.ss"
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
  
  (define lines (readlines "revenge.txt"))
  
  (define hres 24)
  (define i 0)
  (for-each 
   (lambda (line) 
     (let* ((orig (instantiate read-only-text% ()))
            (mine (instantiate text% ()))
            (text (instantiate editor-snip% (orig) (with-border? #f)))
            (edit (instantiate editor-snip% (mine) (min-width 500) (with-border? #f)))
            )
       (if (not (equal? line ""))
           (begin
             (send orig insert-text line)
             (send* p 
               (insert text)
               (move-to text 0 (* i hres))
               (insert edit))
             (set! i (+ i 1))
             (send p move-to edit 0 (* i hres))))
       (set! i (+ i 1))
       ))
   lines)
  
  (send f show #t)
  )