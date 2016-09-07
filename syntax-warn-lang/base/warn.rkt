#lang racket/base

(module+ initial-bindings

  (require (for-syntax racket/base
                       syntax/parse
                       warn
                       warn/require)
           (except-in racket/base require)
           (rename-in racket/base [require base-require])
           racket/splicing
           racket/stxparam)
  
  (define-syntax (require stx)
    (define stx/warn
      (warn-require-phase-order (syntax-local-introduce stx)))
    (define expanded-stx
      (syntax-parse stx
        [(_ spec ...) #'(base-require spec ...)]))
    (datum->syntax expanded-stx
                   (syntax-e expanded-stx)
                   expanded-stx
                   stx/warn))
  
  (provide (all-from-out racket/base)
           require))

(module reader syntax/module-reader
  #:language '(submod racket/base/warn initial-bindings))
