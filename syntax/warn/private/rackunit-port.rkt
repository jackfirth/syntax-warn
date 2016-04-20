#lang typed/racket/base

(provide check-output
         check-output-contains?
         check-error-output
         check-error-output-contains?)

(module untyped racket/base
  (provide check-output
           check-output-contains?
           check-error-output
           check-error-output-contains?)

  (require rackunit racket/port racket/string)

  (define-check (check-output pred a-thunk)
    (define output (with-output-to-string a-thunk))
    (with-check-info (['output output])
                     (unless (pred output)
                       (fail-check))))

  (define-check (check-output-contains? contained a-thunk)
    (define output (with-output-to-string a-thunk))
    (with-check-info (['output output]
                      ['expected-contained contained])
                     (unless (string-contains? output contained)
                       (fail-check))))
  
  (define-check (check-error-output pred a-thunk)
    (define output (with-error-output-to-string a-thunk))
    (with-check-info (['error-output output])
                     (unless (pred output) (fail-check))))

  (define-check (check-error-output-contains? contained a-thunk)
    (define output (with-error-output-to-string a-thunk))
    (with-check-info (['output output]
                      ['expected-contained contained])
                     (unless (string-contains? output contained)
                       (fail-check))))
  
  (define (with-error-output-to-string a-thunk)
    (call-with-output-string
     (lambda (string-port)
       (parameterize ([current-error-port string-port])
         (a-thunk))))))

(require/typed 'untyped
  [check-output (-> (-> String Boolean) (-> Any) Void)]
  [check-output-contains? (-> String (-> Any) Void)]
  [check-error-output (-> (-> String Boolean) (-> Any) Void)]
  [check-error-output-contains? (-> String (-> Any) Void)])
