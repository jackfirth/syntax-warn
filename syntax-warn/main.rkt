#lang racket/base

(provide (struct-out suggested-fix)
         (struct-out syntax-warning)
         syntax-warn
         syntax-warnings-property-key
         syntax-warnings)

(require "private/string-lines.rkt"
         "private/syntax-srcloc.rkt"
         "private/syntax-string.rkt"
         racket/list
         racket/format
         racket/function
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin base-module-begin])
         (for-syntax racket/base))

(module+ test
  (require rackunit))


(struct suggested-fix (original-stx replacement-stx) #:prefab)
(struct syntax-warning (location message fix) #:prefab)

(define (syntax-warn stx message #:fix [replacement-stx #f] #:bad-stx [bad-stx stx])
  (define fix (and replacement-stx (suggested-fix bad-stx replacement-stx)))
  (define warning (syntax-warning (syntax-srcloc bad-stx) message fix))
  (syntax-property stx syntax-warnings-property-key
                   (cons warning (or (syntax-property stx syntax-warnings-property-key) '()))))

(define syntax-warnings-property-key 'warnings)

(define (syntax-warnings stx)
  (define datum (syntax-e stx))
  (remove-duplicates
   (append
    (or (syntax-property stx syntax-warnings-property-key) '())
    (if (list? datum)
        (append-map syntax-warnings (filter syntax? datum))
        '()))))

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
