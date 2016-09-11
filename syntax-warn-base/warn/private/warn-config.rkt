#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [empty-syntax-warning-config syntax-warning-config?]
  [kind-config? flat-contract?]
  [syntax-warning-config
   (->* () (#:kinds kind-config?) syntax-warning-config?)]
  [syntax-warning-config? predicate/c]
  [syntax-warning-config-kinds (-> syntax-warning-config? kind-config?)]
  [syntax-warning-config-merge
   (->* () #:rest (listof syntax-warning-config?) syntax-warning-config?)]))

(require racket/function
         "warn.rkt")

(module+ test
  (require rackunit))


(define kind-config?
  (hash/c warning-kind?
          (or/c 'suppress 'unsuppress)
          #:immutable #t
          #:flat? #t))

(struct syntax-warning-config
  (kinds)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-syntax-warning-config)

(define (syntax-warning-config #:kinds [kind-config (hash)])
  (make-syntax-warning-config kind-config))

(define empty-syntax-warning-config (syntax-warning-config))

(module+ test
  (check-equal? (syntax-warning-config #:kinds (hash))
                (make-syntax-warning-config (hash)))
  (check-equal? (syntax-warning-config)
                (make-syntax-warning-config (hash)))
  (check-equal? (syntax-warning-config)
                empty-syntax-warning-config))

(define (hash-merge merge-values hash1 hash2)
  (for/fold ([merged hash1])
            ([(k v2) (in-hash hash2)])
    (define (update v1) (merge-values v1 v2))
    (hash-update merged k update (thunk v2))))

(define (kind-config-merge-two kinds1 kinds2)
  (hash-merge (lambda (a b) b) kinds1 kinds2))

(define (syntax-warning-config-merge-two config1 config2)
  (define kinds1 (syntax-warning-config-kinds config1))
  (define kinds2 (syntax-warning-config-kinds config2))
  (syntax-warning-config #:kinds (kind-config-merge-two kinds1 kinds2)))

(define (syntax-warning-config-merge . configs)
  (foldl syntax-warning-config-merge-two
         empty-syntax-warning-config
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
      (syntax-warning-config #:kinds (hash kind1 'suppress
                                           kind3 'suppress
                                           kind4 'unsuppress)))
    (define config2
      (syntax-warning-config #:kinds (hash kind2 'unsuppress
                                           kind3 'unsuppress
                                           kind4 'suppress)))
    (define config3
      (syntax-warning-config #:kinds (hash kind5 'suppress)))
    (check-equal? (syntax-warning-config-merge config1 config2 config3)
                  (syntax-warning-config #:kinds (hash kind1 'suppress
                                                       kind2 'unsuppress
                                                       kind3 'unsuppress
                                                       kind4 'suppress
                                                       kind5 'suppress)))))
