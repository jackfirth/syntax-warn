#lang racket/base

(module+ initial-bindings

  (require (for-syntax racket/base
                       syntax/parse
                       warn/require)
           (except-in racket/base require)
           (rename-in racket/base [require base-require])
           racket/splicing
           racket/stxparam
           warn/module)

  (define-syntax (require stx)
    (define stx/warn (warn-require-phase-order stx))
    (define expanded-stx
      (syntax-parse stx
        [(_ spec ...) #'(base-require spec ...)]))
    (datum->syntax expanded-stx
                   (syntax-e expanded-stx)
                   expanded-stx
                   stx/warn))

  (provide (except-out (all-from-out racket/base) #%module-begin)
           (rename-out [module-begin/warn #%module-begin])
           require))

(module reader syntax/module-reader
  #:language '(submod racket/base/warn initial-bindings))
