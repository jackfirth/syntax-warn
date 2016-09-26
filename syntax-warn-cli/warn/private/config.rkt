#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [config-args
   (->* () (#:suppress (listof symbol?) #:unsuppress (listof symbol?))
        config-args?)]
  [config-args? predicate/c]
  [config-args->config (-> config-args? warning-config?)]
  [submod-args
   (->* () (#:name (or/c symbol? #f) #:binding (or/c symbol? #f)) submod-args?)]
  [submod-args? predicate/c]
  [submod-args-name (-> submod-args? symbol?)]
  [submod-args-binding (-> submod-args? symbol?)]
  [submod-args-config
   (-> (or/c module-path? resolved-module-path? module-path-index?)
       submod-args?
       warning-config?)]))

(require racket/function
         syntax/warn)

(module+ test
  (require rackunit))


(struct config-args
  (suppressed-names unsuppressed-names)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-config-args)

(define (config-args #:suppress [suppressed '()]
                     #:unsuppress [unsuppressed '()])
  (make-config-args suppressed unsuppressed))

(struct submod-args
  (binding name)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-submod-args)

(define (submod-args #:binding [binding #f] #:name [name #f])
  (make-submod-args (or binding 'config) (or name 'warning-config)))

(module+ test
  (test-case "config-submod-args"
    (check-equal? (submod-args) (make-submod-args 'config 'warning-config))
    (check-equal? (submod-args #:binding 'foo #:name 'bar)
                  (make-submod-args 'foo 'bar))))

(define (submod-args-config modpath config-args)
  (define submod-name (submod-args-name config-args))
  (define submod-binding (submod-args-binding config-args))
  (with-handlers ([exn:fail? (const empty-warning-config)])
    (dynamic-require (list 'submod modpath submod-name)
                     submod-binding
                     (thunk empty-warning-config))))

(define (config-args->config args)
  (define suppressed (apply suppress (config-args-suppressed-names args)))
  (define unsuppressed (apply unsuppress (config-args-unsuppressed-names args)))
  (warning-config-merge suppressed unsuppressed))
