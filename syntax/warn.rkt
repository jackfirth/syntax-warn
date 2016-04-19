#lang typed/racket/base

(provide (struct-out syntax-warning)
         Srcloc
         syntax-warn
         syntax-warnings-property-key
         syntax-warnings
         check-syntax-warnings
         expand-and-check-module)

(require racket/list
         racket/stxparam
         (rename-in (only-in racket/base #%module-begin)
                    [#%module-begin base-module-begin])
         (for-syntax typed/racket/base
                     syntax/parse))

(module+ test
  (require typed/rackunit))


(: syntax-srcloc (-> Syntax Srcloc))
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

(: syntax-warnings (-> Syntax (Listof Syntax-Warning)))
(define (syntax-warnings stx)
  (define datum (syntax-e stx))
  ((inst append Syntax-Warning)
   (or (cast (syntax-property stx syntax-warnings-property-key)
             (Option (Listof Syntax-Warning)))
       '())
   (if (list? datum)
       ((inst append-map Syntax-Warning Syntax) syntax-warnings (filter syntax? datum))
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

(: check-syntax-warnings (-> (Syntaxof Any) Void))
(define (check-syntax-warnings stx) (void))

(: expand-and-check-module (-> (Syntaxof Any) (Syntaxof Any)))
(define (expand-and-check-module stx)
  (define result-stx
    (local-expand stx 'module-begin #f))
  (check-syntax-warnings result-stx)
  result-stx)
