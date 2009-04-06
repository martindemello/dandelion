;;; utility functions

#lang scheme
(provide <> append-to-car collect prefix-lines)

(require scheme/match)

;; <> operator
(define (<> x y)
  (not (= x y)))

;; append a string to the first string in a list of strings
(define (append-to-car str lst)
  (cond [(null? lst) (list str)]
        [else (cons (string-append (car lst) "\n" str) (cdr lst))]))

;; thanks to Matthias Felleisen on the plt mailing list
(define (collect iport)
  (define (cut line) (substring line 2))
  (define (drive N) (if (eof-object? N) '() (gather-same N)))
  (define (gather-same str)
    (define C (string-ref str 0))
    (let L ([str str])
      (define N (read-line iport))
      (cond
        [(eof-object? N) (list str)]
        [(char=? (string-ref N 0) C) (L (string-append str "\n" (cut N)))]
        [else (cons str (drive N))])))
  (define all (drive (read-line iport)))
  (define (select str) (lambda (line) (string=? str (substring line 0 2))))
  (values (list->vector (map cut (filter (select "# ") all)))
          (list->vector (map cut (filter (select "= ") all)))))

(define (prefix-lines prefix string)
  (string-join (map (λ (s) (string-append prefix s)) (regexp-split #rx"\n" string)) "\n"))
