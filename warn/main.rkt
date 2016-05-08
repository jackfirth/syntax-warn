#lang racket/base

(provide (struct-out syntax-warning)
         syntax-warn
         syntax-warnings-property-key
         syntax-warnings
         check-syntax-warnings
         expand-and-check-module)

(require "private/rackunit-port.rkt"
         "private/string-pad.rkt"
         racket/list
         racket/function
         racket/string
         racket/stxparam
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin base-module-begin])
         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit))


(define (syntax-srcloc stx)
  (make-srcloc (syntax-source stx)
               (syntax-line stx)
               (syntax-column stx)
               (syntax-position stx)
               (syntax-span stx)))

(struct syntax-warning (location message)
  #:transparent)

(define (syntax-warn stx message)
  (syntax-property stx syntax-warnings-property-key
                   (cons (syntax-warning (syntax-srcloc stx) message)
                         (or (syntax-property stx syntax-warnings-property-key) '()))))

(define syntax-warnings-property-key 'warnings)

(define (syntax-warnings stx)
  (define datum (syntax-e stx))
  (append
   (or (syntax-property stx syntax-warnings-property-key)
       '())
   (if (list? datum)
       (append-map syntax-warnings (filter syntax? datum))
       '())))

(module+ test
  (define orig-stx #'(lambda lambda lambda))
  (define first-message "Shadowing the language defined identifier \"lambda\" is discouraged")
  (define warned-once-stx
    (syntax-warn orig-stx first-message))
  (define second-message "This function is identicial to the built in \"identity\" procedure")
  (check-equal? (syntax-warnings (syntax-warn warned-once-stx second-message))
                (list (syntax-warning (syntax-srcloc warned-once-stx) second-message)
                      (syntax-warning (syntax-srcloc orig-stx) first-message))))

(define (srcloc-location-string srcloc)
  (format "~a:~a:~a"
          (srcloc-source srcloc)
          (srcloc-line srcloc)
          (srcloc-column srcloc)))

(define (check-syntax-warnings stx)
  (define warnings (syntax-warnings stx))
  (define max-location-string-length
    (apply max
           (map (compose (compose string-length srcloc-location-string)
                         syntax-warning-location)
                warnings)))
  (for ([warning (in-list (syntax-warnings stx))])
    (define location-string (srcloc-location-string (syntax-warning-location warning)))
    (eprintf "~a ~a"
             (string-pad-right location-string #\space max-location-string-length)
             (syntax-warning-message warning))))

(module+ test
  (check-error-output-contains? "not there"
    (thunk (check-syntax-warnings (syntax-warn #'here "not there")))))

(define (expand-and-check-module stx)
  (define result-stx
    (local-expand stx 'module-begin #f))
  (check-syntax-warnings result-stx)
  result-stx)
