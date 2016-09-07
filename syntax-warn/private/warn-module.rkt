#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [read-module-warnings (-> path-string? (listof syntax-warning?))]))

(require racket/function
         syntax/modread
         "warn.rkt"
         "warn-property.rkt")

(module+ test
  (require racket/runtime-path
           rackunit))


(define (read-module-warnings modpath)
  (define (read-modpath-expansion)
    (with-input-from-file modpath #:mode 'text
      (thunk
       (port-count-lines! (current-input-port))
       (define-values (moddir _1 _2) (split-path modpath))
       (parameterize ([current-namespace (make-base-namespace)]
                      [current-directory moddir])
         (expand-syntax
          (namespace-syntax-introduce
           (read-syntax modpath)))))))
  (syntax-warnings
   (with-module-reading-parameterization read-modpath-expansion)))

(module+ test
  (define-runtime-path test-mod-path "warn-module-test.rkt")
  (test-case "read-module-warnings"
    (check-equal? (length (read-module-warnings test-mod-path)) 1)))
