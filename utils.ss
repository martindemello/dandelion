;;; utility functions

(module utils scheme/base
  (provide <>)
  
  ;; <> operator
  (define (<> x y)
    (not (= x y)))
  
  
  )
