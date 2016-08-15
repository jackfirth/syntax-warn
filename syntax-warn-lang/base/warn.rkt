#lang racket/base

(module+ initial-bindings

  (require (for-syntax racket/base
                       syntax/parse
                       warn/require)
           racket/base
           racket/splicing
           racket/stxparam
           warn/module)

  (define-syntax (require/warn stx)
    (syntax-parse stx
      [(_ spec ...)
       (require-syntax-warn #'(require spec ...)
                            (syntax->list #'(spec ...))
                            #:bad-stx stx
                            #:require-id #'require)]))


  (provide (except-out (all-from-out racket/base) #%module-begin require)
           (rename-out [module-begin/warn #%module-begin]
                       [require/warn require])))

(module reader syntax/module-reader
  #:language '(submod racket/base/warn initial-bindings))
