;;; utility functions

(module utils mzscheme
  (provide <>)
  
  ;; <> operator
  (define (<> x y)
    (not (= x y)))
  
  
  )
