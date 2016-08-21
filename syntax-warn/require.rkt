#lang racket/base

(provide require-syntax-warn)

(require (for-template racket/base)
         syntax/parse
         "main.rkt"
         "private/rackunit-syntax.rkt")

(module+ test
  (require rackunit))

(define (phase-require-spec-sort-priority spec)
  (syntax-parse spec
    #:literals (for-syntax for-template for-label for-meta)
    [(for-syntax . _) 0]
    [(for-template . _) 1]
    [(for-label . _) 2]
    [(for-meta . _) 3]
    [_ 4]))

(module+ test
  (check-equal? (phase-require-spec-sort-priority #'(for-syntax foo)) 0)
  (check-equal? (phase-require-spec-sort-priority #'(for-template foo)) 1)
  (check-equal? (phase-require-spec-sort-priority #'foo) 4)
  (check-equal? (phase-require-spec-sort-priority #'"foo.rkt") 4))

(define (phase-ordered-specs require-specs)
  (sort require-specs < #:key phase-require-spec-sort-priority))

(module+ test
  (define specs
    (list #'foo #'"bar.rkt"
          #'(for-template foo)
          #'(for-syntax bar)
          #'(for-syntax baz)))
  (check-syntax-datum-equal? (datum->syntax #f (phase-ordered-specs specs))
                             #'((for-syntax bar)
                                (for-syntax baz)
                                (for-template foo)
                                foo
                                "bar.rkt")))

(define phase-order-message
  (string-append
   "Require specs are not in phase order "
   "(phase order is syntax, template, label, meta, then plain)"))

(define (require-syntax-warn stx require-specs
                             #:bad-stx [bad-stx stx]
                             #:require-id [require-id #'require])
  (define ordered-specs (phase-ordered-specs require-specs))
  (if (equal? require-specs ordered-specs)
      stx
      (syntax-warn stx phase-order-message
                   #:fix #`(#,require-id #,@ordered-specs)
                   #:bad-stx bad-stx)))
