#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [complete-srcloc
   (->* ()
        (#:source any/c
         #:position exact-positive-integer?
         #:position-span exact-nonnegative-integer?
         #:line exact-positive-integer?
         #:line-span exact-nonnegative-integer?
         #:column exact-nonnegative-integer?
         #:column-end exact-nonnegative-integer?)
        complete-srcloc?)]
  [complete-srcloc? predicate/c]
  [complete-srcloc-set
   (->* (complete-srcloc?)
        (#:source any/c
         #:position exact-positive-integer?
         #:position-span exact-nonnegative-integer?
         #:line exact-positive-integer?
         #:line-span exact-nonnegative-integer?
         #:column exact-nonnegative-integer?
         #:column-end exact-nonnegative-integer?)
        complete-srcloc?)]
  [complete-srcloc-source? srcloc-has->/c]
  [complete-srcloc-source (-> complete-srcloc? any/c)]
  [complete-srcloc-position? srcloc-has->/c]
  [complete-srcloc-position pos-srcloc->]
  [complete-srcloc-position-end pos-srcloc->]
  [complete-srcloc-position-span nonneg-srcloc->]
  [complete-srcloc-line+col? srcloc-has->/c]
  [complete-srcloc-line pos-srcloc->]
  [complete-srcloc-line-end pos-srcloc->]
  [complete-srcloc-line-span nonneg-srcloc->]
  [complete-srcloc-column nonneg-srcloc->]
  [complete-srcloc-column-end nonneg-srcloc->]
  [complete-srcloc->srcloc (-> complete-srcloc? srcloc?)]
  [syntax-set-srcloc (-> syntax? complete-srcloc? syntax?)]))

(require racket/bool
         syntax/parse/define)

(module+ test
  (require racket/format
           racket/function
           rackunit))


(define (srcloc-custom-write loc port mode)
  (define recur
    (case mode
      [(#t) write]
      [(#f) display]
      [else (lambda (p port) (print p port mode))]))
  (write-string "(complete-srcloc" port)
  (define (write-field field-name field-getter)
    (when (field-getter loc)
      (write-string " #:" port)
      (write-string field-name port)
      (write-string " " port)
      (recur (field-getter loc) port)))
  (write-field "source" complete-srcloc-source)
  (write-field "position" complete-srcloc-position)
  (write-field "position-span" complete-srcloc-position-span)
  (write-field "line" complete-srcloc-line)
  (write-field "line-span" complete-srcloc-line-span)
  (write-field "column" complete-srcloc-column)
  (write-field "column-end" complete-srcloc-column-end)
  (write-string ")" port))

(struct complete-srcloc
  (source position position-span line line-span column column-end)
  #:constructor-name make-complete-srcloc
  #:omit-define-syntaxes
  #:transparent
  #:methods gen:custom-write
  [(define write-proc srcloc-custom-write)])

(module+ test
  (check-equal? (~a (complete-srcloc #:source "/foo/bar.rkt"
                                     #:position 10
                                     #:position-span 100
                                     #:line 3
                                     #:line-span 2
                                     #:column 4
                                     #:column-end 12))
                "(complete-srcloc\
 #:source /foo/bar.rkt\
 #:position 10\
 #:position-span 100\
 #:line 3\
 #:line-span 2\
 #:column 4\
 #:column-end 12)")
  (check-equal? (~v (complete-srcloc #:source "/foo/bar.rkt"
                                     #:position 10
                                     #:position-span 100
                                     #:line 3
                                     #:line-span 2
                                     #:column 4
                                     #:column-end 12))
                "(complete-srcloc\
 #:source \"/foo/bar.rkt\"\
 #:position 10\
 #:position-span 100\
 #:line 3\
 #:line-span 2\
 #:column 4\
 #:column-end 12)"))

(define srcloc-has->/c (-> complete-srcloc? boolean?))

(define pos-srcloc->
  (-> complete-srcloc? (or/c exact-positive-integer? #f)))

(define nonneg-srcloc->
  (-> complete-srcloc? (or/c exact-nonnegative-integer? #f)))

(define-simple-macro
  (define-srcloc-predicates [pred-id:id getter-id:id] ...)
  (begin
    (define (pred-id v) (not (not (getter-id v)))) ...))

(define-srcloc-predicates
  [complete-srcloc-source? complete-srcloc-source]
  [complete-srcloc-position? complete-srcloc-position]
  [complete-srcloc-line+col? complete-srcloc-line])

(define (complete-srcloc #:source [s #f]
                         #:position [p #f]
                         #:position-span [ps #f]
                         #:line [l #f]
                         #:line-span [ls #f]
                         #:column [c #f]
                         #:column-end [ce #f])
  (define (check-either-both-or-neither-provided arg1 arg2 name1 name2)
    (when (xor arg1 arg2)
      (raise-arguments-error
       'complete-srcloc
       (format "Either both ~a and ~a must be provided or neither"
               name1 name2)
       name1 arg1 name2 arg2)))
  (check-either-both-or-neither-provided p ps "position" "position-span")
  (check-either-both-or-neither-provided l ls "line" "line-span")
  (check-either-both-or-neither-provided c ce "column" "column-end")
  (check-either-both-or-neither-provided l c "line" "column")
  (make-complete-srcloc s p ps l ls c ce))

(module+ test
  (check-exn exn:fail:contract?
             (thunk (complete-srcloc #:position 1)))
  (check-exn exn:fail:contract?
             (thunk (complete-srcloc #:position-span 1)))
  (check-not-exn (thunk (complete-srcloc #:position 1 #:position-span 1)))
  (check-not-exn (thunk (complete-srcloc)))
  (check-exn exn:fail:contract?
             (thunk (complete-srcloc #:line 1 #:line-span 1)))
  (check-exn exn:fail:contract?
             (thunk (complete-srcloc #:column 1 #:column-span 1)))
  (check-not-exn (thunk (complete-srcloc #:line 1 #:line-span 1
                                         #:column 1 #:column-end 1))))


(define (complete-srcloc-position-end loc)
  (and (complete-srcloc-position? loc)
       (+ (complete-srcloc-position loc)
          (complete-srcloc-position-span loc))))

(module+ test
  (test-case "complete-srcloc-position-end"
    (define test-loc
      (complete-srcloc #:position 10 #:position-span 40))
    (check-equal? (complete-srcloc-position-end test-loc) 50)))

(define (complete-srcloc-line-end loc)
  (and (complete-srcloc-line+col? loc)
       (+ (complete-srcloc-line loc)
          (complete-srcloc-line-span loc))))

(module+ test
  (test-case "complete-srcloc-line-end"
    (define test-loc
      (complete-srcloc #:line 5 #:line-span 12
                       #:column 10 #:column-end 52))
    (check-equal? (complete-srcloc-line-end test-loc) 17)))

(define (complete-srcloc-set loc
                             #:source [s #f]
                             #:position [p #f]
                             #:position-span [ps #f]
                             #:line [l #f]
                             #:line-span [ls #f]
                             #:column [c #f]
                             #:column-end [ce #f])
  (define (arg-or v getter)
    (or v (getter loc)))
  (complete-srcloc #:source (arg-or s complete-srcloc-source)
                   #:position (arg-or p complete-srcloc-position)
                   #:position-span (arg-or ps complete-srcloc-position-span)
                   #:line (arg-or l complete-srcloc-line)
                   #:line-span (arg-or ls complete-srcloc-line-span)
                   #:column (arg-or c complete-srcloc-column)
                   #:column-end (arg-or ce complete-srcloc-column-end)))

(module+ test
  (test-case "complete-srcloc-set"
    (define test-loc
      (complete-srcloc #:source "foo"))
    (check-equal? (complete-srcloc-set test-loc
                                       #:position 5
                                       #:position-span 10)
                  (complete-srcloc #:source "foo"
                                   #:position 5
                                   #:position-span 10))))

(define (complete-srcloc->srcloc loc)
  (srcloc (complete-srcloc-source loc)
          (complete-srcloc-line loc)
          (complete-srcloc-column loc)
          (complete-srcloc-position loc)
          (complete-srcloc-position-span loc)))

(module+ test
  (test-case "complete-srcloc->srcloc"
    (define test-loc
      (complete-srcloc #:source "foo"
                       #:position 10
                       #:position-span 20
                       #:line 2
                       #:line-span 3
                       #:column 0
                       #:column-end 5))
    (check-equal? (complete-srcloc->srcloc test-loc)
                  (srcloc "foo" 2 0 10 20))))

(define (syntax-set-srcloc stx loc)
  (define list-loc
    (list (complete-srcloc-source loc)
          (complete-srcloc-line loc)
          (complete-srcloc-column loc)
          (complete-srcloc-position loc)
          (complete-srcloc-position-span loc)))
  (datum->syntax stx (syntax-e stx) list-loc stx))

(module+ test
  (test-case "syntax-set-srcloc"
    (define test-loc
      (complete-srcloc #:source "foo"
                       #:position 10
                       #:position-span 20
                       #:line 2
                       #:line-span 3
                       #:column 0
                       #:column-end 5))
    (define test-stx/loc
      (syntax-set-srcloc #'here test-loc))
    (check-equal? (syntax-source test-stx/loc) "foo")
    (check-equal? (syntax-line test-stx/loc) 2)
    (check-equal? (syntax-column test-stx/loc) 0)
    (check-equal? (syntax-position test-stx/loc) 10)
    (check-equal? (syntax-span test-stx/loc) 20)))
