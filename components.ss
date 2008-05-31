;;; gui components
(module components mzscheme
  (provide orig-style read-only-text% editable-text%)
  (require "utils.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss"))
  
  (define orig-style 
    (send* (make-object style-delta% 'change-style 'italic)
      (set-delta-foreground "indigo")
      (set-delta-background "ghostwhite")
      ))
  
  (define read-only-text% 
    (class text:hide-caret/selection%
      (inherit lock insert hide-caret change-style last-line)
      (init-field (initial-text ""))
      (define/public (insert-text text)
        (lock #f)
        (insert text 0)
        (lock #t))
      (define/public (nlines)
        (+ 1 (last-line)))
      ;; initialize
      (super-instantiate ())
      (hide-caret #t)      
      (change-style orig-style)
      (insert-text initial-text)
      (lock #t)
      ))
  
  (define editable-text%
    (class text%
      (inherit last-line)
      (define/public (nlines)
        (+ 1 (last-line)))
      ;; initialize
      (super-instantiate ())
      ))
  
  
  )