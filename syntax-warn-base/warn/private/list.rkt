#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [list*? predicate/c]
  [list*->list (-> list*? list?)]))

(require racket/list
         racket/match)

(module+ test
  (require rackunit))


(define (list*? v)
  (or (empty? v) (pair? v)))

(define (list*->list pairs)
  (match pairs
    [(list) (list)]
    [(cons v (? list? vs))
     (cons v (list*->list vs))]
    [(cons v1 v2) (list v1 v2)]))

(module+ test
  (check-equal? (list*->list '()) '())
  (check-equal? (list*->list '(a b)) '(a b))
  (check-equal? (list*->list '(a . b)) '(a b)))
