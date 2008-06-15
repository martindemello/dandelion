;;; utility functions

(module utils scheme/base
  (provide <> append-to-car)
  
  ;; <> operator
  (define (<> x y)
    (not (= x y)))
  
  ;; append a string to the first string in a list of strings
  (define (append-to-car str lst)
    (cond [(null? lst) (list str)]
          [else (cons (string-append (car lst) "\n" str) (cdr lst))]))
  )
