#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [empty-warning-config warning-config?]
  [suppressions-config? flat-contract?]
  [suppress (->rest warning-kind? warning-config?)]
  [unsuppress (->rest warning-kind? warning-config?)]
  [warning-config (->* () (#:suppressions suppressions-config?)
                       warning-config?)]
  [warning-config? predicate/c]
  [warning-config-suppressions (-> warning-config? suppressions-config?)]
  [warning-config-merge (->rest warning-config? warning-config?)]))

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
    (define-warning-kind kind1)
    (define-warning-kind kind2)
    (define-warning-kind kind3)
    (define-warning-kind kind4)
    (define-warning-kind kind5)
    (define config1
      (warning-config #:suppressions (hash kind1 'suppress
                                           kind3 'suppress
                                           kind4 'unsuppress)))
    (define config2
      (warning-config #:suppressions (hash kind2 'unsuppress
                                           kind3 'unsuppress
                                           kind4 'suppress)))
    (define config3
      (warning-config #:suppressions (hash kind5 'suppress)))
    (check-equal? (warning-config-merge config1 config2 config3)
                  (warning-config #:suppressions (hash kind1 'suppress
                                                       kind2 'unsuppress
                                                       kind3 'unsuppress
                                                       kind4 'suppress
                                                       kind5 'suppress)))))

(define ((suppressions-syntax-warnings-config setting) . kinds)
  (define suppressions-config
    (for/hash ([kind (in-list kinds)])
      (values kind setting)))
  (warning-config #:suppressions suppressions-config))

(define suppress (suppressions-syntax-warnings-config 'suppress))
(define unsuppress (suppressions-syntax-warnings-config 'unsuppress))

(module+ test
  (test-case "suppression sugar procedures"
    (define-warning-kind kind1)
    (define-warning-kind kind2)
    (test-equal? "suppress" (suppress kind1 kind2)
                 (warning-config #:suppressions (hash kind1 'suppress
                                                      kind2 'suppress)))
    (test-equal? "unsuppress" (unsuppress kind1 kind2)
                 (warning-config #:suppressions (hash kind1 'unsuppress
                                                      kind2 'unsuppress)))))
