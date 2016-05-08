#lang racket/base

(provide (struct-out suggested-fix)
         (struct-out syntax-warning)
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

(struct suggested-fix (original-stx replacement-stx) #:transparent)
(struct syntax-warning (location message fix) #:transparent)

(define (syntax-warn stx message #:fix [replacement-stx #f])
  (define fix (and replacement-stx (suggested-fix stx replacement-stx)))
  (define warning (syntax-warning (syntax-srcloc stx) message fix))
  (syntax-property stx syntax-warnings-property-key
                   (cons warning (or (syntax-property stx syntax-warnings-property-key) '()))))

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
  (define orig-stx #'(lambda (lambda) lambda))
  (define first-message "Shadowing the language defined identifier \"lambda\" is discouraged")
  (define warned-once-stx
    (syntax-warn orig-stx first-message))
  (define expected-first-warning (syntax-warning (syntax-srcloc orig-stx) first-message #f))
  (check-equal? (syntax-warnings warned-once-stx) (list expected-first-warning))
  (define second-message "This function is identicial to the built in \"identity\" procedure")
  (define identity-stx #'identity)
  (define warned-twice-stx
    (syntax-warn warned-once-stx second-message #:fix identity-stx))
  (define expected-second-warning (syntax-warning (syntax-srcloc warned-once-stx)
                                                  second-message
                                                  (suggested-fix warned-once-stx identity-stx)))
  (check-equal? (syntax-warnings warned-twice-stx)
                (list expected-second-warning expected-first-warning)))

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
    (eprintf "~a ~a\n"
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
