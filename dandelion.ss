;;; Here’s the basic problem: you’re writing a text editor. Stop doing that. It’s 2007. --  Mark Pilgrim

(module dandelion mzscheme
  
  (require "utils.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss"))
  
  (define read-only-text% 
    (class text:hide-caret/selection%
      (init-field (insertable? #t))
      (define/public (insert-text text)
        (send this lock #f)
        (send this insert text 0)
        (send this lock #t))
      ;; initialize
      (super-instantiate ())
      (send this hide-caret #t)
      
      (send this change-style 
            (send (make-object style-delta% 'change-style 'italic)
                  set-delta-foreground "gray"))
      (send this lock #t)
      ))
  
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
            (edit (instantiate editor-snip% (mine) (min-width 500)))
            )
       (if (not (equal? line ""))
           (begin
             (send orig insert-text line)
             (send mine change-style 
                   (send (make-object style-delta% 'change-style 'normal)
                         set-delta-background "SlateGray"))
             (send p insert text)
             (send p move-to text 0 (* i hres))
             (send p insert edit)
             (set! i (+ i 1))
             (send p move-to edit 0 (* i hres))))
       (set! i (+ i 1))
       ))
   lines)
  
  (send f show #t)
  )