#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [syntax->string
   (->* (syntax?)
        (#:start-col exact-nonnegative-integer?)
        string?)]
  [syntax->string/line-numbers
   (->* (syntax?)
        (#:indent-spaces exact-nonnegative-integer?)
        string?)]))

(require racket/contract
         racket/list
         racket/format
         racket/function
         racket/port
         racket/sequence
         racket/string
         "syntax-srcloc.rkt")

(module+ test
  (require rackunit))


(define (syntax->string/line-numbers stx #:indent-spaces [indent-spaces 1])
  (add-line-numbers (syntax->string stx #:start-col 0)
                    #:start-line (syntax-line stx)
                    #:indent-spaces indent-spaces))

(define (syntax->string stx #:start-col [start-col #f])
  (cond [(syntax-list? stx)
         (syntax-list->string stx #:start-col start-col)]
        [else (literal->string stx #:start-col start-col)]))

(define (syntax-list? stx)
  (not (not (syntax->list stx))))

(define (literal->string stx #:start-col [start-col #f])
  (string-append
   (if start-col (make-string (- (syntax-column stx) start-col) #\space) "")
   (~s (syntax->datum stx))))

(define (syntax-list->string stx #:start-col [start-col #f])
  (define loc (syntax-complete-srcloc stx))
  (define col (complete-srcloc-column loc))
  (with-output-to-string
      (thunk
       (when start-col
         (write-spaces (- col start-col)))
       (write-string "(")
       (for/fold ([prev-end-col (add1 col)]
                  [prev-end-line (complete-srcloc-line loc)])
                 ([stx-part (in-syntax stx)])
         (define part-loc (syntax-complete-srcloc stx-part))
         (define part-line (complete-srcloc-line part-loc))
         (define part-col (complete-srcloc-column part-loc))
         (if (equal? prev-end-line part-line)
             (write-spaces (- part-col prev-end-col))
             (begin
               (write-newlines (- part-line prev-end-line))
               (write-spaces part-col)))
         (write-string
          (syntax->string stx-part #:start-col part-col))
         (values (complete-srcloc-column-end part-loc)
                 (complete-srcloc-line-end part-loc)))
       (write-string ")"))))

(define (add-line-numbers str
                          #:start-line [start-line 1]
                          #:indent-spaces [indent-spaces 1])
  (define lines
    (string-split str "\n" #:trim? #f #:repeat? #t))
  (define max-line-digits
    (num-digits (+ start-line (length lines))))
  (define (format-line line n)
    (string-append (~a n #:min-width max-line-digits)
                   (make-string indent-spaces #\space)
                   line))
  (string-join
   (for/list ([line lines]
              [n (in-naturals start-line)])
     (format-line line n))
   "\n"))

(define (num-digits n)
  (string-length (~a n)))

(define (write-char chr n)
  (void (write-string (make-string n chr))))

(define/contract (write-spaces n)
  (-> exact-nonnegative-integer? void?)
  (write-char #\space n))

(define/contract (write-newlines n)
  (-> exact-nonnegative-integer? void?)
  (write-char #\newline n))

(define-syntax-rule (with-output-to-string* body ...)
  (with-output-to-string (thunk body ...)))

(module+ test
  (define test-stx
    #'(  require foo
         bar
                 (baz   blah
                    )
               bork   fork))
  (check-equal? (syntax->string test-stx)
                "(  require foo\n         bar\n                 (baz   blah)\n\n               bork   fork)")
  (check-equal? (syntax->string test-stx #:start-col 0)
                "      (  require foo\n         bar\n                 (baz   blah)\n\n               bork   fork)"))
