;;; Here’s the basic problem: you’re writing a text editor. Stop doing that. It’s 2007. --  Mark Pilgrim

(require "utils.ss"
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
(send f show #t)

(define lines (readlines "revenge.txt"))

(define i 0)
(for-each 
 (lambda (line) 
   (let* ((t (instantiate text% ()))
          (s (instantiate editor-snip% (t)))
          )
     (send t insert line 0)
     (send p insert s)
     (send p move-to s 0 (* i 20))
     (set! i (+ i 1))
     ))
 lines)
