#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [read-syntax-warnings
   (->* ()
        (#:source-name any/c
         #:input-port input-port?
         #:namespace namespace?)
        (listof syntax-warning?))]
  [read-syntax-warnings/file
   (->* (path-string?)
        (#:namespace namespace?)
        (listof syntax-warning?))]))

(require racket/function
         syntax/modread
         "warn.rkt"
         "warn-property.rkt")

(module+ test
  (require racket/runtime-path
           rackunit))


(define (read-syntax-warnings #:source-name [maybe-source-name #f]
                              #:input-port [maybe-input-port #f]
                              #:namespace [maybe-namespace #f])
  (define input-port (or maybe-input-port (current-input-port)))
  (define source-name (or maybe-source-name (object-name input-port)))
  (define namespace (or maybe-namespace (current-namespace)))
  (syntax-warnings
   (with-module-reading-parameterization
       (thunk
        (parameterize ([current-namespace namespace])
          (expand-syntax
           (namespace-syntax-introduce
            (read-syntax source-name input-port))))))))

(define (read-syntax-warnings/file filepath
                                   #:namespace [maybe-namespace #f])
  (with-input-from-file filepath #:mode 'text
    (thunk
     (port-count-lines! (current-input-port))
     (define-values (filedir ignored-1 ignored-2)
       (split-path filepath))
     (parameterize ([current-directory filedir])
       (read-syntax-warnings #:source-name filepath
                             #:namespace maybe-namespace)))))


(module+ test
  (define-runtime-path test-mod-path "warn-module-test.rkt")
  (test-case "read-syntax-warnings/file"
    (define test-warnings
      (read-syntax-warnings/file test-mod-path
                                 #:namespace (make-base-namespace)))
    (check-equal? (length test-warnings) 1)))
