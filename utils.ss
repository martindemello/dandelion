;;; utility functions

#lang scheme/base
(provide <> append-to-car file->vectors)

(require scheme/match)

;; <> operator
(define (<> x y)
  (not (= x y)))

;; append a string to the first string in a list of strings
(define (append-to-car str lst)
  (cond [(null? lst) (list str)]
        [else (cons (string-append (car lst) "\n" str) (cdr lst))]))

(define (file->vectors filename)
  ; read in file into two parallel vectors
  (let [(input-file (open-input-file filename))]
    (let-values 
        ([(read-o read-p _)
          (for/fold ([o '()] [p '()] [c "# "])
            ([line (in-lines input-file)])
            (match (list (substring line 0 2) (substring line 2))
              [(list "# " rest)
               (cond [(equal? c "# ") (values (append-to-car rest o) p "# ")]
                     [else            (values (cons rest o) p "# ")])]
              [(list "= " rest)
               (cond [(equal? c "= ") (values o (append-to-car rest p) "= ")]
                     [else            (values o (cons rest p) "= ")])]))])
      (close-input-port input-file)
      (list (list->vector (reverse read-o)) (list->vector (reverse read-p))))))

