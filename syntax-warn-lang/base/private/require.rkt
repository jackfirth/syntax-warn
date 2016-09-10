#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [warn-require-phase-order (-> syntax? syntax?)]))

(require (for-template racket/base)
         racket/match
         syntax/parse
         syntax/warn
         syntax/warn/private/rackunit-syntax
         syntax/warn/private/syntax-format
         syntax/warn/private/syntax-srcloc)

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

(define-warning-kind require-phase-order)

(define phase-order-message
  (string-append
   "Require specs are not in phase order "
   "(phase order is syntax, template, label, meta, then plain)"))

(define (warn-require-phase-order stx)
  (syntax-parse stx
    [(req-id:id clause ...)
     (define clause-stxs (syntax->list #'(clause ...)))
     (define clause-stxs/order (phase-ordered-specs clause-stxs))
     (cond
       [(equal? clause-stxs clause-stxs/order) stx]
       [else
        (define warning
          (syntax-warning #:message phase-order-message
                          #:kind require-phase-order
                          #:stx stx
                          #:fix (build-formatted-phase-ordered-stx stx)))
        (syntax-warn stx warning)])]))

(define (build-formatted-phase-ordered-stx stx)
  (syntax-parse stx
    [(req-id:id clause ...)
     (define clause-stxs (syntax->list #'(clause ...)))
     (define clause-stxs/order (phase-ordered-specs clause-stxs))
     (define stxs (cons #'req-id clause-stxs/order))
     (syntax-list->syntax/srcloc stxs #:parent stx)]))

(define (syntax-reformat stx)
  (define stxs (syntax->list stx))
  (if stxs
      (syntax-set-srcloc/squeeze-to-fit-children
       (syntax-list->syntax/srcloc stxs #:parent stx))
      stx))

(define (syntax-list->syntax/srcloc stxs #:parent stx)
  (match stxs
    [(list) (datum->syntax stx stxs stx stx)]
    [(list single)
     (define single/loc-top
       (syntax-set-srcloc/first-child single #:parent stx))
     (define stxs/loc (list (syntax-reformat single/loc-top)))
     (datum->syntax stx stxs/loc stx stx)]
    [(list* first second rest)
     (define first/loc
       (syntax-reformat
        (syntax-set-srcloc/first-child first #:parent stx)))
     (define second/loc
       (syntax-reformat
        (syntax-set-srcloc/follow-same-line second #:follow first/loc)))
     (define-values (rest/loc/reversed _)
       (for/fold ([rest/loc/reversed '()]
                  [last-stx second/loc])
                 ([stx rest])
         (define stx/loc
           (syntax-reformat
            (syntax-set-srcloc/follow-next-line stx #:follow last-stx)))
         (values (cons stx/loc rest/loc/reversed) stx/loc)))
     (define stxs/loc
       (list* first/loc second/loc (reverse rest/loc/reversed)))
     (datum->syntax stx stxs/loc stx stx)]))
