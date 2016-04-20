#lang typed/racket/base

(provide (struct-out syntax-warning)
         Srcloc
         syntax-warn
         syntax-warnings-property-key
         syntax-warnings
         check-syntax-warnings)

(require typed/racket/unsafe)

(unsafe-provide expand-and-check-module)

(require "warn/private/rackunit-port.rkt"
         "warn/private/string-pad.rkt"
         racket/list
         racket/function
         racket/stxparam
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin base-module-begin])
         (for-syntax typed/racket/base
                     syntax/parse))

(require/typed racket/string
               [string-contains? (-> String String Boolean)])

(module+ test
  (require typed/rackunit))


(: syntax-srcloc (-> (Syntaxof Any) Srcloc))
(define (syntax-srcloc stx)
  (make-srcloc (syntax-source stx)
               (syntax-line stx)
               (syntax-column stx)
               (syntax-position stx)
               (syntax-span stx)))

(define-type Srcloc srcloc)
(struct syntax-warning ([location : Srcloc] [message : String])
  #:transparent #:type-name Syntax-Warning)

(: syntax-warn (-> Syntax String Syntax))
(define (syntax-warn stx message)
  (syntax-property stx syntax-warnings-property-key
                   (cons (syntax-warning (syntax-srcloc stx) message)
                         (or (syntax-property stx syntax-warnings-property-key) '()))))

(: syntax-warnings-property-key Any)
(define syntax-warnings-property-key 'warnings)

(: syntax-warnings (-> (Syntaxof Any) (Listof Syntax-Warning)))
(define (syntax-warnings stx)
  (define datum (syntax-e stx))
  ((inst append Syntax-Warning)
   (or (cast (syntax-property stx syntax-warnings-property-key)
             (Option (Listof Syntax-Warning)))
       '())
   (if (list? datum)
       ((inst append-map Syntax-Warning (Syntaxof Any)) syntax-warnings (filter syntax? datum))
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

(: srcloc-location-string (-> Srcloc String))
(define (srcloc-location-string srcloc)
  (format "~a:~a:~a"
          (srcloc-source srcloc)
          (srcloc-line srcloc)
          (srcloc-column srcloc)))

(: check-syntax-warnings (-> (Syntaxof Any) Void))
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

(: expand-and-check-module (-> (Syntaxof Any) (Syntaxof Any)))
(define (expand-and-check-module stx)
  (define result-stx
    (local-expand stx 'module-begin #f))
  (check-syntax-warnings result-stx)
  result-stx)
