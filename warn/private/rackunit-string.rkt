#lang racket/base

(provide check-string-contains?
         check-string-contains-all?
         check-string-has-trailing-newline?)

(require racket/list
         racket/string
         rackunit)


(define-check (check-string-contains? str expected)
  (with-check-info (['string str]
                    ['expected-substring expected])
    (unless (string-contains? str expected)
      (fail-check))))

(define-check (check-string-contains-all? str expecteds)
  (define (contains? expected) (string-contains? str expected))
  (define-values (contained not-contained) (partition contains? expecteds))
  (with-check-info (['string str]
                    ['contained contained]
                    ['not-contained not-contained])
    (unless (empty? not-contained)
      (fail-check))))

(define-check (check-string-has-trailing-newline? str)
  (with-check-info (['string str])
    (unless (string-suffix? str (string #\newline))
      (fail-check))))
