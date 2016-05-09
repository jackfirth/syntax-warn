#lang racket/base

(provide require/warn
         require/warn/shadow)

(require racket/stxparam
         (for-syntax racket/base
                     syntax/parse
                     warn/require))

(define-syntax (require/warn/shadow stx)
  (syntax-parse stx
    [(_ spec ...)
     (require-syntax-warn #'(require spec ...)
                          (syntax->list #'(spec ...))
                          #:bad-stx stx
                          #:require-id #'require)]))
