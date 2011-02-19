#lang scheme

(provide hierarchical-undo-stack%)

;; side effect (x:xs) (ys) -> (xs) (x:ys) 
;; returns x
(define-syntax rotate-stacks
  (syntax-rules ()
    ((rotate-stacks l r)
     (if (empty? l) 
         '()
         (let ([retval (car l)])
           (set! l (cdr l))
           (set! r (cons retval r))
           retval)))))

(define undo-stack%
  (class object%
    (init-field undo-fn)
    (init-field redo-fn)
    (define undo-stack '())
    (define redo-stack '())
    
    (define (pop-undo)
      (rotate-stacks undo-stack redo-stack))
    
    (define (pop-redo)
      (rotate-stacks redo-stack undo-stack))
    
    ; adding an undo clears the redo stack
    (define/public (add-undo e)      
      (print "adding to stack")
      (set! undo-stack (cons e undo-stack))
      (set! redo-stack '()))
    
    (define/public (undo)
      (let ([e (pop-undo)])
        (unless (empty? e) (undo-fn e))))
    
    (define/public (redo)
      (let ([e (pop-redo)])
        (unless (empty? e) (redo-fn e))))
    
    (super-instantiate ())))

;; undo stack of objects that respond to undo and redo
(define hierarchical-undo-stack%
  (class undo-stack%
    (super-new
     (undo-fn (λ (e) (send e undo)))
     (redo-fn (λ (e) (send e redo))))))