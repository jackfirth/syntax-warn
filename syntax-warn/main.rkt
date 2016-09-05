#lang racket/base

(provide (struct-out suggested-fix)
         (struct-out syntax-warning)
         read-module-warnings
         syntax-warn
         syntax-warning/fix?
         syntax-warnings)

(require (for-syntax racket/base)
         "private/syntax-srcloc.rkt"
         racket/function
         racket/list
         syntax/modread)

(module+ test
  (require rackunit))


(struct suggested-fix (original-stx replacement-stx) #:prefab)
(struct syntax-warning (location message fix) #:prefab)

(define (syntax-warning/fix? v)
  (and (syntax-warning? v)
       (not (not (syntax-warning-fix v)))))

(define (syntax-warn stx message #:fix [replacement-stx #f] #:bad-stx [bad-stx stx])
  (define fix (and replacement-stx (suggested-fix bad-stx replacement-stx)))
  (define warning (syntax-warning (syntax-srcloc bad-stx) message fix))
  (define warnings (syntax-property stx syntax-warnings-property-key))
  (define warnings/added (cons warning (or warnings '())))
  (syntax-property stx syntax-warnings-property-key warnings/added))

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
