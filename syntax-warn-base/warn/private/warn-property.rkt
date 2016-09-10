#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [syntax-warn (-> syntax? syntax-warning? syntax?)]
  [syntax-warnings (-> syntax? (listof syntax-warning?))]))

(require racket/list
         "syntax-srcloc.rkt"
         "warn.rkt")

(module+ test
  (require rackunit))


(define (syntax-warn stx warning)
  (define (cons-warning warnings)
    (cons warning (or warnings '())))
  (syntax-property-transform stx warnings-key cons-warning))

(define (syntax-warnings stx)
  (define datum (syntax-e stx))
  (remove-duplicates
   (append
    (or (syntax-property stx warnings-key) '())
    (if (list? datum)
        (append-map syntax-warnings (filter syntax? datum))
        '()))))

(define (syntax-property-transform stx prop-key f)
  (syntax-property stx prop-key (f (syntax-property stx prop-key))))

(define warnings-key 'warnings)

(module+ test
  (test-case "syntax-warnings"
    (define-warning-kind test-kind)
    (define test-stx #'(test foo bar (baz blah) bloop))
    (define test-stx/fix
    (datum->syntax test-stx 'florp test-stx test-stx))
    (define first-warning
      (syntax-warning #:message "Test warning one"
                      #:kind test-kind
                      #:stx test-stx))
    (define second-warning
      (syntax-warning #:message "Test warning two"
                      #:kind test-kind
                      #:stx test-stx
                      #:fix test-stx/fix))
    (define warned-once (syntax-warn test-stx first-warning))
    (define warned-twice (syntax-warn warned-once second-warning))
    (check-equal? (syntax-warnings warned-once) (list first-warning))
    (check-equal? (syntax-warnings warned-twice)
                  (list second-warning first-warning))))
