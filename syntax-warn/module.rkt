#lang racket/base

(provide warned-module-begin
         module-begin/warn)

(require racket/stxparam
         (for-syntax syntax/parse
                     warn
                     racket/base))


(define-syntax-parameter warned-module-begin
  (make-rename-transformer #'#%module-begin))

(define-syntax (module-begin/warn stx)
  (syntax-parse stx
    [(_ body ...)
     (expand-and-check-module #'(warned-module-begin body ...))]))
