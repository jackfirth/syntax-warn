#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [struct string-delta
    ([start exact-nonnegative-integer?]
     [span exact-nonnegative-integer?]
     [new-text string?])]
  [string-delta-end (-> string-delta? exact-nonnegative-integer?)]
  [prune-string-deltas (-> (listof string-delta?)
                           (listof string-delta?))]
  [apply-pruned-string-deltas (-> string? (listof string-delta?)
                                  string?)]))

(require racket/list
         syntax/warn/private/syntax-srcloc)

(module+ test
  (require rackunit))


(struct string-delta (start span new-text) #:transparent)

(module+ test
  (define delta (string-delta 10 24 "blah"))
  (define engulfed (string-delta 12 5 "foo"))
  (define overlapped (string-delta 30 20 "kdlfjs"))
  (define before (string-delta 3 5 "klf"))
  (define after (string-delta 45 23 "lkfdjalkf")))


(define (string-delta-end delta)
  (+ (string-delta-start delta) (string-delta-span delta)))

(module+ test
  (check-equal? (string-delta-end delta) 34))

(define ((string-delta-compare compare-proc field-proc) . deltas)
  (or (empty? deltas)
      (empty? (rest deltas))
      (apply compare-proc (map field-proc deltas))))

(define string-delta-start< (string-delta-compare < string-delta-start))
(define string-delta-start> (string-delta-compare > string-delta-start))
(define string-delta-start= (string-delta-compare = string-delta-start))
(define string-delta-start<= (string-delta-compare <= string-delta-start))
(define string-delta-start>= (string-delta-compare >= string-delta-start))
(define string-delta-end< (string-delta-compare < string-delta-end))
(define string-delta-end> (string-delta-compare > string-delta-end))
(define string-delta-end= (string-delta-compare = string-delta-end))
(define string-delta-end<= (string-delta-compare <= string-delta-end))
(define string-delta-end>= (string-delta-compare >= string-delta-end))

(module+ test
  (test-case "string-delta-start<"
    (test-true "empty case" (string-delta-start<))
    (test-true "single argument case" (string-delta-start< delta))
    (test-case "multi argument cases"
      (check-true (string-delta-start< delta engulfed))
      (check-false (string-delta-start< engulfed delta))
      (check-true (string-delta-start< before delta engulfed overlapped after))
      (check-false (string-delta-start< before delta overlapped engulfed after)))))

(define (string-delta-engulfs? delta engulfed-delta)
  (and (string-delta-start<= delta engulfed-delta)
       (string-delta-end>= delta engulfed-delta)))

(module+ test
  (test-case "string-delta-engulfs?"
    (define (test-string-delta-engulfs? test-delta)
      (string-delta-engulfs? delta test-delta))
    (define (test-string-delta-engulfs-not? test-delta)
      (not (string-delta-engulfs? delta test-delta)))
    (check-pred test-string-delta-engulfs? engulfed)
    (check-pred test-string-delta-engulfs-not? overlapped)
    (check-pred test-string-delta-engulfs-not? before)
    (check-pred test-string-delta-engulfs-not? after)
    (check-pred test-string-delta-engulfs? delta)))

(define (string-delta-overlaps? delta overlapped-delta)
  (if (string-delta-start<= delta overlapped-delta)
      (<= (string-delta-start overlapped-delta)
          (string-delta-end delta))
      (string-delta-overlaps? overlapped-delta delta)))

(module+ test
  (test-case "string-delta-overlaps?"
    (define (test-string-delta-overlaps? test-delta)
      (string-delta-overlaps? delta test-delta))
    (define (test-string-delta-overlaps-not? test-delta)
      (not (string-delta-overlaps? delta test-delta)))
    (check-pred test-string-delta-overlaps? engulfed)
    (check-pred test-string-delta-overlaps? overlapped)
    (check-pred test-string-delta-overlaps-not? before)
    (check-pred test-string-delta-overlaps-not? after)
    (check-pred test-string-delta-overlaps? delta)))

;; Given a list of file deltas, one of them can't be applied if:
;; - Another delta engulfs it
;; - Another delta overlaps it
;; If a delta engulfs another delta, the engulfing delta may be
;; applied (note this makes engulfs assymetric but overlaps
;; symmetric w.r.t. pruning)
(define (prune-string-deltas deltas)
  (cond
    [(empty? deltas) '()]
    [else
     (define sorted (sort deltas string-delta-start<=))
     (define (keep? delta previous-delta)
       (and (not (string-delta-engulfs? previous-delta delta))
            (not (string-delta-overlaps? previous-delta delta))))
     (reverse
      (for/fold ([previous-deltas '()])
                ([delta (in-list sorted)])
        (cond
          [(for/or ([previous-delta (in-list previous-deltas)])
             (string-delta-engulfs? previous-delta delta))
           previous-deltas]
          [(for/or ([previous-delta (in-list previous-deltas)])
             (string-delta-engulfs? delta previous-delta))
           (define (keep? previous-delta)
             (not (string-delta-engulfs? delta previous-delta)))
           (cons delta (filter keep? previous-deltas))]
          [(for/or ([previous-delta (in-list previous-deltas)])
             (string-delta-overlaps? previous-delta delta))
           (define (keep? previous-delta)
             (not (string-delta-overlaps? previous-delta delta)))
           (filter keep? previous-deltas)]
          [else (cons delta previous-deltas)])))]))

(module+ test
  (test-case "prune-string-deltas"
    (define non-overlapping-deltas
      (list (string-delta 23 3 "kfj")
            (string-delta 59 24 "flksa")
            (string-delta 10 4 "dkflaj")))
    (define non-overlapping-deltas/sorted
      (list (string-delta 10 4 "dkflaj")
            (string-delta 23 3 "kfj")
            (string-delta 59 24 "flksa")))
    (check-equal? (prune-string-deltas non-overlapping-deltas)
                  non-overlapping-deltas/sorted)
    (define engulfed-deltas
      (list (string-delta 10 40 "sfklaj")
            (string-delta 20 4 "sklafj")
            (string-delta 2 6 "slakfj")
            (string-delta 56 23 "saklfj")))
    (define engulfed-deltas/sorted+engulfed-removed
      (list (string-delta 2 6 "slakfj")
            (string-delta 10 40 "sfklaj")
            (string-delta 56 23 "saklfj")))
    (check-equal? (prune-string-deltas engulfed-deltas)
                  engulfed-deltas/sorted+engulfed-removed)
    (define overlapped-deltas
      (list (string-delta 23 10 "ksalfj")
            (string-delta 18 6 "asfklja")
            (string-delta 5 10 "asklfjaslf")
            (string-delta 40 29 "asklfjas")))
    (define overlapped-deltas/sorted+overlapped-removed
      (list (string-delta 5 10 "asklfjaslf")
            (string-delta 40 29 "asklfjas")))
    (check-equal? (prune-string-deltas overlapped-deltas)
                  overlapped-deltas/sorted+overlapped-removed)))

(define (apply-pruned-string-deltas str deltas)
  (cond
    [(empty? deltas) str]
    [else
     (define-values (pieces/reversed last-end)
       (for/fold ([pieces '()]
                  [prev-end #f])
                 ([delta (in-list deltas)])
         (define start (string-delta-start delta))
         (define end (string-delta-end delta))
         (define next-piece (substring str (or prev-end 0) start))
         (values (cons next-piece pieces) end)))
     (define last-piece (substring str last-end))
     (define all-pieces (reverse (cons last-piece pieces/reversed)))
     (define-values (all-pieces/insertions/reversed _)
       (for/fold ([all-pieces/insertions/reversed (list (first all-pieces))]
                  [prev-piece (first all-pieces)])
                 ([piece (in-list (rest all-pieces))]
                  [delta (in-list deltas)])
         (values (list* piece
                        (string-delta-new-text delta)
                        all-pieces/insertions/reversed)
                 piece)))
     (apply string-append (reverse all-pieces/insertions/reversed))]))

(module+ test
  (test-case "apply-pruned-string-deltas"
    (define deltas
      (list (string-delta 1 5 "REPLACED")
            (string-delta 8 3 "X")
            (string-delta 15 5 "FOO")))
    (check-equal? (apply-pruned-string-deltas "foo bar baz blah bleh" deltas)
                  "fREPLACEDr X blaFOOh")))
