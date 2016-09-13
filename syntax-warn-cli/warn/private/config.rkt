#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [submod-config-args
   (->* () (#:submod-name (or/c symbol? #f) #:binding-name (or/c symbol? #f))
        submod-config-args?)]
  [submod-config-args? predicate/c]
  [submod-config-args-submod (-> submod-config-args? symbol?)]
  [submod-config-args-binding (-> submod-config-args? symbol?)]
  [require-warning-config-submod
   (-> (or/c module-path? resolved-module-path? module-path-index?)
       submod-config-args?
       warning-config?)]))

(require racket/function
         syntax/warn/private/warn-config)

(module+ test
  (require rackunit))


(struct submod-config-args
  (submod binding)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-submod-config-args)

(define (submod-config-args #:submod-name [submod #f]
                            #:binding-name [binding #f])
  (make-submod-config-args (or submod 'warning-config)
                           (or binding 'config)))

(module+ test
  (test-case "config-submod-args"
    (check-equal? (submod-config-args)
                  (make-submod-config-args 'warning-config 'config))
    (check-equal? (submod-config-args #:submod-name 'foo
                                      #:binding-name 'bar)
                  (make-submod-config-args 'foo 'bar))))

(define (require-warning-config-submod modpath config-args)
  (define submod-name (submod-config-args-submod config-args))
  (define binding-name (submod-config-args-binding config-args))
  (with-handlers ([exn:fail? (const empty-warning-config)])
    (dynamic-require (list 'submod modpath submod-name)
                     binding-name
                     (thunk empty-warning-config))))
