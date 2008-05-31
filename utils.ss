;;; utility functions

(module utils mzscheme
  (provide readlines <>)
  
  ;; filename -> (lines)
  (define (readlines filename)
    (call-with-input-file filename
      (lambda (p)
        (let loop ((line (read-line p))
                   (result '()))
          (if (eof-object? line)
              (reverse result)
              (loop (read-line p) (cons line result)))))))
  
  ;; <> operator
  (define (<> x y)
    (not (= x y)))
  
  
  )
