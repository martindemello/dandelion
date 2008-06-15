;;; gui components
(module components scheme
  (provide orig-style-delta read-only-text% editable-text%)
  (require "utils.ss"
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss"))
  
  (define orig-style-delta
    (send* (make-object style-delta% 'change-style 'italic)
      (set-delta-background "lightblue")
      (set-delta-foreground "indigo")
      ))
  
  (send (send the-style-list new-named-style "Original" (send the-style-list basic-style))
   set-delta orig-style-delta)
  
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
      (insert-text initial-text)
      (lock #t)
      ))
  
  (define editable-text%
    (class text%
      (init-field on-height-changed)
      (init-field next-editor)
      (init-field prev-editor)
      (init-field set-active)
      (init-field (initial-text ""))
      ;; height acts as a cache for the number of lines
      ;; so we can tell if insert/delete has changed it
      (define height 0)
      (define (set-height) (set! height (last-line)))
      (define (check-height)
        (when (<> height (last-line)) (on-height-changed)))
      (inherit last-line position-line get-start-position insert)
      (define (current-line) (position-line (get-start-position)))
      (define (last-line?) (= (current-line) (last-line)))
      (define (first-line?) (= (current-line) 0))
      (define/public (nlines) (+ 1 (last-line)))
      (define/augment (on-insert a b) (set-height))
      (define/augment (on-delete a b) (set-height))
      (define/augment (after-insert a b) (check-height))
      (define/augment (after-delete a b) (check-height))
      (define/override (on-focus on?) (when on? (set-active)))
      (define/override (on-local-char key)
        (let ([code (send key get-key-code)])
          (cond [(and (equal? code 'down) (last-line?)) (next-editor) ]
                [(and (equal? code 'up) (first-line?)) (prev-editor) ]
                [else (super on-local-char key)])))
      
      ;; initialize
      (super-instantiate ())
      (insert initial-text 0)
      ))
  )