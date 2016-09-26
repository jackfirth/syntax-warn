#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [empty-warning-config warning-config?]
  [suppress (->rest (or/c warning-kind? symbol?) warning-config?)]
  [unsuppress (->rest (or/c warning-kind? symbol?) warning-config?)]
  [warning-config? predicate/c]
  [warning-config-merge (->rest warning-config? warning-config?)]
  [filter-unsuppressed-warnings
   (-> (listof syntax-warning?) warning-config? (listof syntax-warning?))]))

(require racket/function
         "warn.rkt")

(module+ test
  (require rackunit))


(define (->rest arg-contract result-contract)
  (->* () #:rest (listof arg-contract) result-contract))

(define suppressions-config?
  (hash/c warning-kind?
          (or/c 'suppress 'unsuppress)
          #:immutable #t
          #:flat? #t))

(struct warning-config
  (suppressions)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-warning-config)

(define (warning-config #:suppressions [suppressions-config (hash)])
  (make-warning-config suppressions-config))

(define empty-warning-config (warning-config))

(module+ test
  (check-equal? (warning-config #:suppressions (hash))
                (make-warning-config (hash)))
  (check-equal? (warning-config) (make-warning-config (hash)))
  (check-equal? (warning-config) empty-warning-config))

(define (hash-merge merge-values hash1 hash2)
  (for/fold ([merged hash1])
            ([(k v2) (in-hash hash2)])
    (define (update v1) (merge-values v1 v2))
    (hash-update merged k update (thunk v2))))

(define (suppressions-config-merge-two suppressions1 suppressions2)
  (hash-merge (lambda (a b) b) suppressions1 suppressions2))

(define (syntax-warning-config-merge-two config1 config2)
  (define suppressions1 (warning-config-suppressions config1))
  (define suppressions2 (warning-config-suppressions config2))
  (warning-config #:suppressions (suppressions-config-merge-two suppressions1
                                                        suppressions2)))

(define (warning-config-merge . configs)
  (foldl syntax-warning-config-merge-two
         empty-warning-config
         (reverse configs)))

(module+ test
  (require (submod ".."))
  (test-case "syntax-warning-config-merge"
    (define config1
      (warning-config #:suppressions (hash 'kind1 'suppress
                                           'kind3 'suppress
                                           'kind4 'unsuppress)))
    (define config2
      (warning-config #:suppressions (hash 'kind2 'unsuppress
                                           'kind3 'unsuppress
                                           'kind4 'suppress)))
    (define config3
      (warning-config #:suppressions (hash 'kind5 'suppress)))
    (check-equal? (warning-config-merge config1 config2 config3)
                  (warning-config #:suppressions (hash 'kind1 'suppress
                                                       'kind2 'unsuppress
                                                       'kind3 'unsuppress
                                                       'kind4 'suppress
                                                       'kind5 'suppress)))))

(define ((suppressions-syntax-warnings-config setting) . kinds-or-names)
  (define names
    (for/list ([kind-or-name (in-list kinds-or-names)])
      (if (warning-kind? kind-or-name)
          (warning-kind-name kind-or-name)
          kind-or-name)))
  (define suppressions-config
    (for/hash ([name (in-list names)])
      (values name setting)))
  (warning-config #:suppressions suppressions-config))

(define suppress (suppressions-syntax-warnings-config 'suppress))
(define unsuppress (suppressions-syntax-warnings-config 'unsuppress))

(module+ test
  (test-case "suppression sugar procedures"
    (define-warning-kind kind1)
    (define-warning-kind kind2)
    (test-equal? "suppress" (suppress kind1 'kind2)
                 (warning-config #:suppressions (hash 'kind1 'suppress
                                                      'kind2 'suppress)))
    (test-equal? "unsuppress" (unsuppress 'kind1 kind2)
                 (warning-config #:suppressions (hash 'kind1 'unsuppress
                                                      'kind2 'unsuppress)))))

(define (filter-unsuppressed-warnings warnings config)
  (define (keep? warning)
    (define maybe-kind (syntax-warning-kind warning))
    (or (not maybe-kind)
        (equal? (hash-ref (warning-config-suppressions config)
                          (warning-kind-name maybe-kind)
                          'unsuppress)
                'unsuppress)))
  (filter keep? warnings))

(module+ test
  (test-case "filter-unsuppressed-warnings"
    (define-warning-kind kind1)
    (define-warning-kind kind2)
    (define warning/no-kind
      (syntax-warning #:message "Test warning without kind"
                      #:stx #'here))
    (define warning/kind1
      (syntax-warning #:message "Test warning kind1"
                      #:stx #'there
                      #:kind kind1))
    (define warning/kind2
      (syntax-warning #:message "Test warning kind2"
                      #:stx #'where
                      #:kind kind2))
    (define warnings
      (list warning/no-kind warning/kind1 warning/kind2))
    (check-equal? (filter-unsuppressed-warnings warnings (suppress kind1))
                  (list warning/no-kind warning/kind2))
    (check-equal? (filter-unsuppressed-warnings warnings (unsuppress kind2))
                  warnings)
    (check-equal? (filter-unsuppressed-warnings warnings (suppress kind1 kind2))
                  (list warning/no-kind))))
