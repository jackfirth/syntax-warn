#lang racket/base

(provide check-syntax-datum-equal?)

(require rackunit)

(define-check (check-syntax-datum-equal? actual expected)
  (define actual-datum (syntax->datum actual))
  (define expected-datum (syntax->datum expected))
  (with-check-info (['actual actual-datum]
                    ['expected expected-datum])
    (unless (equal? actual-datum expected-datum)
      (fail-check))))
