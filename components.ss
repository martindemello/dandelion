;;; gui components

#lang scheme

(provide orig-style-delta read-only-text% editable-text%)
(require "utils.ss"
         mred framework scheme/class)

(define orig-style-delta
  (send* (make-object style-delta% 'change-style 'italic)
    (set-delta-background "lightblue")
    (set-delta-foreground "indigo")
    ))

(define editor-keymap
  (let ((keymap (make-object keymap%)))
    (add-text-keymap-functions keymap)
    (send keymap map-function "c:left" "backward-word")
    (send keymap map-function "c:right" "forward-word")
    (send keymap map-function "s:c:left" "backward-select-word")
    (send keymap map-function "s:c:right" "forward-select-word")
    (send keymap map-function "c:insert" "copy-clipboard")
    (send keymap map-function "s:insert" "paste-clipboard")
    (send keymap map-function "c:z" "undo")
    (send keymap map-function "c:y" "redo")
    keymap))

(send (send the-style-list new-named-style "Original" (send the-style-list basic-style))
      set-delta orig-style-delta)

(define read-only-text% 
  (class text:hide-caret/selection%
    (inherit lock insert hide-caret change-style last-line set-keymap)
    (init-field (initial-text ""))
    (define/public (insert-text text)
      (lock #f)
      (insert text 0)
      (lock #t))
    (define/public (nlines) (+ 1 (last-line)))
    ;; initialize
    (super-instantiate ())
    (set-keymap editor-keymap)
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
    (inherit last-line position-line get-start-position insert set-keymap set-max-undo-history clear-undos)
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
    (set-keymap editor-keymap)
    (set-max-undo-history 'forever)
    (insert initial-text 0)
    ))
