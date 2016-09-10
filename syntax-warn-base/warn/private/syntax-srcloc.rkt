#lang racket/base

(require racket/contract/base
         "syntax-srcloc-data.rkt")

(provide
 (all-from-out "syntax-srcloc-data.rkt")
 (contract-out
  [tree/c (-> contract? contract?)]
  [tree-map (-> (tree/c any/c) procedure? (tree/c any/c))]
  [syntax-srcloc (-> syntax? srcloc?)]
  [syntax-complete-srcloc (-> syntax? complete-srcloc?)]
  [syntax-complete-srcloc-tree (-> syntax? (tree/c complete-srcloc?))]))

(require racket/list
         racket/match
         syntax/srcloc
         syntax/parse/define
         "list.rkt")

(module+ test
  (require racket/function
           rackunit))

(define (tree/c item/c)
  (or/c empty?
        (recursive-contract (cons/c item/c (listof (tree/c item/c))))))

(module+ test
  (test-case "tree/c"
    (define (x-tree? v)
      (with-handlers ([exn:fail:contract? (const #f)])
        (invariant-assertion (tree/c 'x) v)
        #t))
    (check-pred x-tree? '())
    (check-pred x-tree? '(x))
    (check-pred x-tree? '(x () () ()))
    (check-pred x-tree? '(x (x (x)) () (x () (x) ())))
    (define (not-x-tree? v)
      (not (x-tree? v)))
    (check-pred not-x-tree? '(a))
    (check-pred not-x-tree? '(x x))
    (check-pred not-x-tree? '(x () x))))

(define (tree-map a-tree f)
  (match a-tree
    [(list) (list)]
    [(cons v children)
     (define (recur child) (tree-map child f))
     (cons (f v) (map recur children))]))

(module+ test
  (test-case "tree-map"
    (check-equal? (tree-map '(1 (2) (3 (4) (5)) (6)) add1)
                  '(2 (3) (4 (5) (6)) (7)))))

(define (syntax-srcloc stx) (build-source-location stx))

(define (syntax-complete-srcloc-tree stx)
  (cons (syntax-complete-srcloc stx)
        (map syntax-complete-srcloc-tree
             (or (syntax->list stx) '()))))

(define (syntax-complete-srcloc stx)
  (check-syntax-srcloc-complete stx)
  (define datum (syntax-e stx))
  (cond
    [(pair? datum) (syntax-complete-srcloc/pair stx)]
    [(vector? datum) (syntax-complete-srcloc/vector stx)]
    [(box? datum) (syntax-complete-srcloc/box stx)]
    [(hash? datum) (syntax-complete-srcloc/hash stx)]
    [else (syntax-complete-srcloc/single-line stx)]))

(define (syntax-complete-srcloc/single-line stx)
  (define-srcloc-values (source pos line col span) stx)
  (complete-srcloc #:source source
                   #:position pos
                   #:position-span span
                   #:line line
                   #:line-span 0
                   #:column col
                   #:column-end (+ col span)))

(define (syntax-complete-srcloc/pair stx)
  (define incomplete-parentloc (build-source-location stx))
  (define child-srclocs
    (map syntax-complete-srcloc (list*->list (syntax-e stx))))
  (check-child-srclocs-valid stx child-srclocs incomplete-parentloc)

  (define firstloc (first child-srclocs))
  (define lastloc (last child-srclocs))

  (define source (complete-srcloc-source firstloc))
  (define position (srcloc-position incomplete-parentloc))
  (define position-span
    (add1 (- (complete-srcloc-position-end lastloc) position)))
  (define line (srcloc-line incomplete-parentloc))
  (define line-span
    (- (complete-srcloc-line-end lastloc) line))
  (define column (srcloc-column incomplete-parentloc))
  (define column-end (add1 (complete-srcloc-column-end lastloc)))

  (complete-srcloc #:source source
                   #:position position
                   #:position-span position-span
                   #:line line
                   #:line-span line-span
                   #:column column
                   #:column-end column-end))

(define (syntax-complete-srcloc/vector stx) #f)
(define (syntax-complete-srcloc/box stx) #f)
(define (syntax-complete-srcloc/hash stx) #f)

(define-simple-macro
  (define-srcloc-values (source:id pos:id line:id col:id span:id)
    source-location:expr)
  (define-values (source pos line col span)
    (let ([loc source-location])
      (values (source-location-source loc)
              (source-location-position loc)
              (source-location-line loc)
              (source-location-column loc)
              (source-location-span loc)))))

(define (check-syntax-srcloc-complete stx)
  (define loc (build-source-location stx))
  (define (check-field name getter)
    (unless (getter loc)
      (raise-arguments-error
       'syntax-complete-srcloc
       "syntax source location is incomplete"
       "syntax" stx
       "location" loc
       "missing" name)))
  (check-field "source" srcloc-source)
  (check-field "position" srcloc-position)
  (check-field "line" srcloc-line)
  (check-field "column" srcloc-column)
  (check-field "span" srcloc-span))

(define (check-child-srclocs-valid stx locs incomplete-parentloc)
  (define (check-child-field-after loc field-name field-getter field-baseline)
    (define field-value (field-getter loc))
    (unless (< field-baseline field-value)
      (raise-arguments-error
       'syntax-complete-srcloc
       (format "child has ~a not after parent ~a" field-name field-name)
       "syntax" stx
       (format "parent ~a" field-name) field-baseline
       (format "child ~a" field-name) field-value)))
  (define (check-child-field-not-before loc field-name field-getter field-baseline)
    (define field-value (field-getter loc))
    (unless (<= field-baseline field-value)
      (raise-arguments-error
       'syntax-complete-srcloc
       (format "child has ~a before parent ~a" field-name field-name)
       "syntax" stx
       (format "parent ~a" field-name) field-baseline
       (format "child ~a" field-name) field-value)))
  (for/fold ([position (srcloc-position incomplete-parentloc)]
             [line (srcloc-line incomplete-parentloc)]
             [column (srcloc-column incomplete-parentloc)])
            ([loc locs])
    (check-child-field-after loc "position" complete-srcloc-position position)
    (check-child-field-not-before loc "line" complete-srcloc-line line)
    (when (equal? line (complete-srcloc-line loc))
      (check-child-field-after loc "column" complete-srcloc-column column))
    (values (complete-srcloc-position loc)
            (complete-srcloc-line loc)
            (complete-srcloc-column loc)))
  (void))
