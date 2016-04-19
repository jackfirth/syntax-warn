#lang typed/racket/base

(provide warned-module-begin
         module-begin/warn)

(require (for-syntax syntax/parse
                     syntax/warn
                     typed/racket/base))


(define-syntax-parameter warned-module-begin
  (make-rename-transformer #'base-module-begin))

(define-syntax (module-begin/warn stx)
  (syntax-parse stx
    [(_ body ...)
     (expand-and-check-module #'(warned-module-begin body ...))]))
