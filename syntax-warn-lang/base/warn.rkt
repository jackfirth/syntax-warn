#lang racket/base

(require racket/contract
         "private/require.rkt")

(provide (recontract-out require:phase-order))


(module initial-bindings racket/base

  (require (for-syntax racket/base
                       syntax/parse
                       syntax/warn
                       "private/require.rkt")
           (except-in racket/base require)
           (rename-in racket/base [require base-require])
           racket/splicing
           racket/stxparam)

  (define-syntax (require stx)
    (define stx/warn
      (syntax-warn/require-phase-order (syntax-local-introduce stx)))
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
