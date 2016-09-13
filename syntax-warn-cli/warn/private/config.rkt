#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [config-submod-args
   (->* () (#:submod-name (or/c symbol? #f) #:binding-name (or/c symbol? #f))
        config-submod-args?)]
  [config-submod-args? predicate/c]
  [config-submod-args-submod-name (-> config-submod-args? symbol?)]
  [config-submod-args-binding-name (-> config-submod-args? symbol?)]
  [require-warning-config-submod
   (-> (or/c module-path? resolved-module-path? module-path-index?)
       config-submod-args?
       warning-config?)]))

(require racket/function
         syntax/warn/private/warn-config)

(module+ test
  (require rackunit))


(struct config-submod-args
  (submod-name binding-name)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-config-submod-args)

(define (config-submod-args #:submod-name [submod-name #f]
                            #:binding-name [binding-name #f])
  (make-config-submod-args (or submod-name 'warning-config)
                           (or binding-name 'config)))

(module+ test
  (test-case "config-submod-args"
    (check-equal? (config-submod-args)
                  (make-config-submod-args 'warning-config 'config))
    (check-equal? (config-submod-args #:submod-name 'foo
                                      #:binding-name 'bar)
                  (make-config-submod-args 'foo 'bar))))

(define (require-warning-config-submod modpath config-args)
  (define submod-name (config-submod-args-submod-name config-args))
  (define binding-name (config-submod-args-binding-name config-args))
  (with-handlers ([exn:fail? (const empty-warning-config)])
    (dynamic-require (list 'submod modpath submod-name)
                     binding-name
                     (thunk empty-warning-config))))
