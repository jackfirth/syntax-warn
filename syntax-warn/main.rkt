#lang racket/base

(provide (struct-out suggested-fix)
         (struct-out syntax-warning)
         syntax-warn
         syntax-warnings-property-key
         syntax-warnings
         check-syntax-warnings
         expand-and-check-module)

(require "private/rackunit-port.rkt"
         "private/rackunit-string.rkt"
         "private/string-lines.rkt"
         "private/syntax-string.rkt"
         racket/list
         racket/format
         racket/function
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin base-module-begin])
         (for-syntax racket/base))

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

(define (srcloc-location-string srcloc)
  (format "~a:~a"
          (srcloc-source srcloc)
          (srcloc-line srcloc)))

(define (separator-format sep width)
  (define separator (make-string width sep))
  (string-append-lines separator "~a" separator ""))

(define (format-warning warning #:separator-char [sep #\-] #:separator-width [width 80])
  (define warning-message
    (format "~a ~a"
            (srcloc-location-string (syntax-warning-location warning))
            (syntax-warning-message warning)))
  (define fix (syntax-warning-fix warning))
  (define message-format
    (if fix
        (separator-format sep width)
        "~a\n"))
  (define (indent str) (string-append "  " str))
  (format message-format
          (if fix
              (string-append-lines
               warning-message
               "suggested fix:"
               ""
               (syntax->string (suggested-fix-original-stx fix))
               ""
               "->"
               ""
               (syntax->string (suggested-fix-replacement-stx fix)))
              warning-message)))

(module+ test
  (define formatted-warning
    (format-warning (syntax-warning (syntax-srcloc #'here) "not there" #f)))
  (check-string-contains? formatted-warning "warn/main.rkt")
  (check-string-contains? formatted-warning "not there")
  (check-string-has-trailing-newline? formatted-warning)
  (define formatted-warning/fix
    (format-warning (syntax-warning (syntax-srcloc #'foo) "use a different name"
                                    (suggested-fix #'foo #'bar))))
  (check-string-contains-all? formatted-warning/fix
                              '("----------------"
                                "warn/main.rkt"
                                "use a different name"
                                "suggested fix:"
                                "foo"
                                "->"
                                "bar")))

(define (check-syntax-warnings stx)
  (for ([warning (in-list (syntax-warnings stx))])
    (eprintf (format-warning warning))))

(module+ test
  (check-error-output-contains? "not there"
                                (thunk (check-syntax-warnings (syntax-warn #'here "not there")))))

(define (expand-and-check-module stx)
  (define result-stx
    (local-expand stx 'module-begin (list #'module*)))
  (check-syntax-warnings result-stx)
  result-stx)
