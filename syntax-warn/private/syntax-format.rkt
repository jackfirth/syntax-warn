#lang racket/base

(require racket/contract)

(provide syntax-list->syntax/proc-call-format/newlines)

(require racket/list)

(define (syntax-list->syntax/proc-call-format/newlines
         stxs
         #:start-srcloc-stx start
         #:context ctx
         #:stx-props-stx props)
  (cond
    [(empty? stxs)
     (syntax-set-srcloc #'() start)]
    [else
     (define first-srcloc
       (list (syntax-source start)
             (syntax-line start)
             (add1 (syntax-column start))
             (add1 (syntax-position start))
             (syntax-span (first stxs))))
     (define first-stx/loc
       (syntax-set-srcloc (first stxs) first-srcloc))
     (cond
       [(empty? (rest stxs))
        (define parent-srcloc
          (list (syntax-source start)
                (syntax-line start)
                (syntax-column start)
                (syntax-position start)
                (add1 (fifth first-srcloc))))
        (datum->syntax ctx (list first-stx/loc) parent-srcloc props)]
       [else
        (define second-stx/loc
          (syntax-set-srcloc/follow-same-line (second stxs)
                                              #:follow first-stx/loc))
        (define stxs/loc
          (let loop ([stxs (rest (rest stxs))]
                     [stxs/loc (list second-stx/loc first-stx/loc)])
            (cond
              [(empty? stxs) (reverse stxs/loc)]
              [else
               (define next-stx/loc
                 (syntax-set-srcloc/follow-next-line (first stxs)
                                                     #:follow (first stxs/loc)))
               (loop (rest stxs) (cons next-stx/loc stxs/loc))])))
        (define parent-srcloc
          (list (syntax-source start)
                (syntax-line start)
                (syntax-column start)
                (syntax-position start)
                (+ (sum stxs/loc #:measure syntax-span)
                   (add1 (length stxs/loc)))))
        (datum->syntax ctx stxs/loc parent-srcloc props)])]))

(define (syntax-set-srcloc stx srcloc)
  (datum->syntax stx (syntax-e stx) srcloc stx))

(define (syntax-set-srcloc/follow-same-line stx #:follow stx-to-follow)
  (define offset
    (add1 (syntax-span stx-to-follow)))
  (define source (syntax-source stx-to-follow))
  (define line (syntax-line stx-to-follow))
  (define col (+ (syntax-column stx-to-follow) offset))
  (define pos (+ (syntax-position stx-to-follow) offset))
  (define span (syntax-span stx))
  (syntax-set-srcloc stx (list source line col pos span)))

(define (syntax-set-srcloc/follow-next-line stx #:follow stx-to-follow)
  (define source (syntax-source stx-to-follow))
  (define line (add1 (syntax-line stx-to-follow)))
  (define col (syntax-column stx-to-follow))
  (define pos
    (+ (syntax-position stx-to-follow)
       (syntax-span stx-to-follow)
       col
       1))
  (define span (syntax-span stx))
  (syntax-set-srcloc stx (list source line col pos span)))

(define (sum vs #:measure [measure-proc values])
  (foldl + 0 (map measure-proc vs)))
