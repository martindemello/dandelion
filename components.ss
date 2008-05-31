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
      (define/public (nlines) (+ 1 (last-line)))
      ;; initialize
      (super-instantiate ())
      (hide-caret #t)      
      (change-style orig-style)
      (insert-text initial-text)
      (lock #t)
      ))
  
  (define editable-text%
    (class text%
      (init on-height-changed)
      ;; height acts as a cache for the number of lines
      ;; so we can tell if insert/delete has changed it
      (define height 0)
      (define (set-height) (set! height (last-line)))
      (define (check-height)
        (if (<> height (last-line)) (on-height-changed)))
      (inherit last-line)
      (define/public (nlines) (+ 1 (last-line)))
      (define/augment (on-insert a b) (set-height))
      (define/augment (on-delete a b) (set-height))
      (define/augment (after-insert a b) (check-height))
      (define/augment (after-delete a b) (check-height))
      
      ;; initialize
      (super-instantiate ())
      ))
  )