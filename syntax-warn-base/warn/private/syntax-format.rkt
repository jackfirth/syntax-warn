#lang racket/base

(require racket/contract)

(provide syntax-set-srcloc/follow-same-line
         syntax-set-srcloc/follow-next-line
         syntax-set-srcloc/first-child
         syntax-set-srcloc/squeeze-to-fit-children)

(require racket/list
         "list.rkt"
         "syntax-srcloc.rkt")

(define (syntax-set-srcloc/follow-same-line stx #:follow stx-to-follow)
  (define follow-loc (syntax-complete-srcloc stx-to-follow))
  (define loc (syntax-complete-srcloc stx))
  (define line-span (complete-srcloc-line-span loc))
  (define new-column (add1 (complete-srcloc-column-end follow-loc)))
  (define column-delta (- (complete-srcloc-column loc) new-column))
  (define old-column-end (complete-srcloc-column-end loc))
  (define new-loc
    (complete-srcloc #:source (complete-srcloc-source follow-loc)
                     #:position (add1 (complete-srcloc-position-end follow-loc))
                     #:position-span (complete-srcloc-position-span loc)
                     #:line (complete-srcloc-line-end follow-loc)
                     #:line-span line-span
                     #:column new-column
                     #:column-end (if (zero? line-span)
                                      (+ column-delta old-column-end)
                                      old-column-end)))
  (syntax-set-srcloc stx new-loc))

(define (syntax-set-srcloc/follow-next-line stx #:follow stx-to-follow)
  (define follow-loc (syntax-complete-srcloc stx-to-follow))
  (define loc (syntax-complete-srcloc stx))
  (define line-span (complete-srcloc-line-span loc))
  (define follow-column (complete-srcloc-column follow-loc))
  (define column-delta (- (complete-srcloc-column loc) follow-column))
  (define old-column-end (complete-srcloc-column-end loc))
  (define new-loc
    (complete-srcloc #:source (complete-srcloc-source follow-loc)
                     #:position (+ (complete-srcloc-position-end follow-loc)
                                   follow-column 1)
                     #:position-span (complete-srcloc-position-span loc)
                     #:line (add1 (complete-srcloc-line-end follow-loc))
                     #:line-span line-span
                     #:column follow-column
                     #:column-end (if (zero? line-span)
                                      (+ column-delta old-column-end)
                                      old-column-end)))
  (syntax-set-srcloc stx new-loc))

(define (syntax-set-srcloc/first-child stx #:parent parent-stx)
  (define parent-loc (syntax-complete-srcloc parent-stx))
  (define loc (syntax-complete-srcloc stx))
  (define line-span (complete-srcloc-line-span loc))
  (define old-column-end (complete-srcloc-column-end loc))
  (define column-delta (- old-column-end (complete-srcloc-column loc)))
  (define new-column (add1 (complete-srcloc-column parent-loc)))
  (define new-loc
    (complete-srcloc #:source (complete-srcloc-source parent-loc)
                     #:position (add1 (complete-srcloc-position parent-loc))
                     #:position-span (complete-srcloc-position-span loc)
                     #:line (complete-srcloc-line parent-loc)
                     #:line-span line-span
                     #:column new-column
                     #:column-end (if (zero? line-span)
                                      (+ new-column column-delta)
                                      old-column-end)))
  (syntax-set-srcloc stx new-loc))

(define (syntax-set-srcloc/squeeze-to-fit-children stx)
  (define children (list*->list (syntax-e stx)))
  (define firstloc (syntax-complete-srcloc (first children)))
  (define lastloc (syntax-complete-srcloc (last children)))
  (define position (sub1 (complete-srcloc-position firstloc)))
  (define position-span
    (- (add1 (complete-srcloc-position-end lastloc)) position))
  (define line (complete-srcloc-line firstloc))
  (define line-span (- (complete-srcloc-line-end lastloc) line))
  (define new-loc
    (complete-srcloc #:source (complete-srcloc-source firstloc)
                     #:position position
                     #:position-span position-span
                     #:line line
                     #:line-span line-span
                     #:column (sub1 (complete-srcloc-column firstloc))
                     #:column-end (add1 (complete-srcloc-column-end lastloc))))
  (syntax-set-srcloc stx new-loc))

(define (display-test-loc #:test-name name
                          #:srcloc-getter [getter complete-srcloc-position-span]
                          #:stx stx)
  (printf "--~a------\n\n" name)
  (displayln (tree-map (syntax-complete-srcloc-tree stx) getter)))
