#lang racket/base

(provide filter/index-result)

(module+ test
  (require rackunit))

(define (filter/index-result p vs)
  (for/list ([v vs] [i (in-naturals)] #:when (p v))
    (list i v)))

(module+ test
  (check-equal? (filter/index-result string? '(1 2 a "b" c "dee" 5 6 "foooo"))
                '((3 "b") (5 "dee") (8 "foooo")))
  (check-equal? (filter/index-result string? '()) '())
  (check-equal? (filter/index-result string? '(1 2 a c 5 6)) '()))
