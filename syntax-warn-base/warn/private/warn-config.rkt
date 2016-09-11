#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [empty-warning-config warning-config?]
  [kind-config? flat-contract?]
  [suppress (->rest warning-kind? warning-config?)]
  [unsuppress (->rest warning-kind? warning-config?)]
  [warning-config (->* () (#:kinds kind-config?) warning-config?)]
  [warning-config? predicate/c]
  [warning-config-kinds (-> warning-config? kind-config?)]
  [warning-config-merge (->rest warning-config? warning-config?)]))

(require racket/function
         "warn.rkt")

(module+ test
  (require rackunit))


(define (->rest arg-contract result-contract)
  (->* () #:rest (listof arg-contract) result-contract))

(define kind-config?
  (hash/c warning-kind?
          (or/c 'suppress 'unsuppress)
          #:immutable #t
          #:flat? #t))

(struct warning-config
  (kinds)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-warning-config)

(define (warning-config #:kinds [kind-config (hash)])
  (make-warning-config kind-config))

(define empty-warning-config (warning-config))

(module+ test
  (check-equal? (warning-config #:kinds (hash)) (make-warning-config (hash)))
  (check-equal? (warning-config) (make-warning-config (hash)))
  (check-equal? (warning-config) empty-warning-config))

(define (hash-merge merge-values hash1 hash2)
  (for/fold ([merged hash1])
            ([(k v2) (in-hash hash2)])
    (define (update v1) (merge-values v1 v2))
    (hash-update merged k update (thunk v2))))

(define (kind-config-merge-two kinds1 kinds2)
  (hash-merge (lambda (a b) b) kinds1 kinds2))

(define (syntax-warning-config-merge-two config1 config2)
  (define kinds1 (warning-config-kinds config1))
  (define kinds2 (warning-config-kinds config2))
  (warning-config #:kinds (kind-config-merge-two kinds1 kinds2)))

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
      (warning-config #:kinds (hash kind1 'suppress
                                    kind3 'suppress
                                    kind4 'unsuppress)))
    (define config2
      (warning-config #:kinds (hash kind2 'unsuppress
                                    kind3 'unsuppress
                                    kind4 'suppress)))
    (define config3
      (warning-config #:kinds (hash kind5 'suppress)))
    (check-equal? (warning-config-merge config1 config2 config3)
                  (warning-config #:kinds (hash kind1 'suppress
                                                kind2 'unsuppress
                                                kind3 'unsuppress
                                                kind4 'suppress
                                                kind5 'suppress)))))

(define ((kinds-syntax-warnings-config setting) . kinds)
  (define kinds-config
    (for/hash ([kind (in-list kinds)])
      (values kind setting)))
  (warning-config #:kinds kinds-config))

(define suppress (kinds-syntax-warnings-config 'suppress))
(define unsuppress (kinds-syntax-warnings-config 'unsuppress))

(module+ test
  (test-case "suppression sugar procedures"
    (define-warning-kind kind1)
    (define-warning-kind kind2)
    (test-equal? "suppress" (suppress kind1 kind2)
                 (warning-config #:kinds (hash kind1 'suppress
                                               kind2 'suppress)))
    (test-equal? "unsuppress" (unsuppress kind1 kind2)
                 (warning-config #:kinds (hash kind1 'unsuppress
                                               kind2 'unsuppress)))))
