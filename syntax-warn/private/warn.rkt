#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [struct warning-kind ([name symbol?])]
  [syntax-warning? predicate/c]
  [syntax-warning/fix? predicate/c]
  [syntax-warning-message (-> syntax-warning? string?)]
  [syntax-warning-kind (-> syntax-warning? warning-kind?)]
  [syntax-warning-stx (-> syntax-warning? syntax?)]
  [syntax-warning-fix (-> syntax-warning? (or/c syntax? #f))]
  [syntax-warning
   (->* (#:message string?
         #:kind warning-kind?
         #:stx (and/c syntax? syntax-original?))
        (#:fix (and/c syntax? syntax-original?))
        syntax-warning?)])
 define-warning-kind)

(require syntax/parse/define)

(module+ test
  (require rackunit))


(struct warning-kind (name) #:prefab)

(define-simple-macro (define-warning-kind name:id)
  (define name (warning-kind 'name)))

(module+ test
  (define-warning-kind test-kind)
  (test-case "define-warning-kind"
    (check-pred warning-kind? test-kind)
    (check-equal? (warning-kind-name test-kind) 'test-kind)))

(struct syntax-warning
  (message kind stx fix)
  #:prefab
  #:omit-define-syntaxes
  #:constructor-name make-syntax-warning)

(define (syntax-warning #:message message
                        #:kind kind
                        #:stx stx
                        #:fix [fix #f])
  (make-syntax-warning message kind stx fix))

(module+ test
  (define here-stx #'here)
  (define here-stx/there
    (datum->syntax here-stx 'there here-stx here-stx))
  (test-case "syntax-warning"
    (test-case "without fix"
      (define test-warning
        (syntax-warning #:message "Test warning"
                        #:kind test-kind
                        #:stx here-stx))
      (check-pred syntax-warning? test-warning)
      (check-equal? (syntax-warning-message test-warning) "Test warning")
      (check-equal? (syntax-warning-kind test-warning) test-kind)
      (check-equal? (syntax-warning-stx test-warning) here-stx)
      (check-false (syntax-warning-fix test-warning)))
    (test-case "with fix"
      (define test-warning/fix
        (syntax-warning #:message "Test warning with fix"
                        #:kind test-kind
                        #:stx here-stx
                        #:fix here-stx/there))
      (check-equal? (syntax-warning-message test-warning/fix) "Test warning with fix")
      (check-equal? (syntax-warning-kind test-warning/fix) test-kind)
      (check-equal? (syntax-warning-stx test-warning/fix) here-stx)
      (check-equal? (syntax-warning-fix test-warning/fix) here-stx/there))))

(define (syntax-warning/fix? v)
  (and (syntax-warning? v)
       (not (not (syntax-warning-fix v)))))

(module+ test
  (test-case "syntax-warning/fix?"
    (check-pred syntax-warning/fix?
                (syntax-warning #:message "Test warning"
                                #:kind test-kind
                                #:stx here-stx
                                #:fix here-stx/there))
    (check-pred (compose not syntax-warning/fix?)
                (syntax-warning #:message "Test warning"
                                #:kind test-kind
                                #:stx here-stx))
    (check-pred (compose not syntax-warning/fix?) 'not-a-warning)))
