#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [syntax->string (-> syntax? string?)]))

(require racket/list
         racket/format
         racket/function
         racket/port
         racket/sequence
         racket/string)

(module+ test
  (require rackunit))


(define (syntax->string stx)
  (cond [(identifier? stx)
         (identifier->string stx)]
        [(syntax-list? stx)
         (syntax-list->string stx)]
        [else (literal->string stx)]))

(define (identifier->string stx)
  (symbol->string (identifier-binding-symbol stx)))

(define (syntax-list? stx)
  (not (not (syntax->list stx))))

(define (write-spaces n)
  (write-string (make-string n #\space)))

(define (write-newlines n)
  (write-string (make-string n #\newline)))

(define (string-indent str n)
  (with-output-to-string*
    (for ([line (string-split str "\n")]
          [i (in-naturals)])
      (write-spaces n)
      (write-string line)
      (unless (zero? i)
        (write-newlines 1)))))

(define (syntax-list->string stx)
  (define start-col (syntax-column stx))
  (unless start-col
    (error 'syntax->string "syntax ~a has no start column" stx))
  (with-output-to-string*
    (write-string "(")
    (for/fold ([prev-end-col (add1 start-col)]
               [prev-line (syntax-line stx)])
              ([stx-part (in-syntax stx)])
      (define part-start-col (syntax-column stx-part))
      (define part-line (syntax-line stx-part))
      (unless part-start-col
        (error 'syntax->string
               "syntax part ~a has no start column"
               stx-part))
      (unless part-line
        (error 'syntax->string
               "syntax part ~a has no line number"))
      (call/comparison
       (compare part-line prev-line)
       #:less (thunk
               (error 'syntax->string
                      "syntax part ~a seems to be on a line earlier than its predecessor"
                      stx-part))
       #:equal (thunk
                (write-spaces (- part-start-col prev-end-col))
                (write-string (syntax->string stx-part)))
       #:greater (thunk
                  (write-newlines (- part-line prev-line))
                  (write-string
                   (string-indent (syntax->string stx-part)
                                  (- part-start-col start-col)))))
      (values (+ part-start-col (syntax-span stx-part)) part-line))
    (write-string ")")))

(define (literal->string stx)
  (~s (syntax->datum stx)))

(define-syntax-rule (with-output-to-string* body ...)
  (with-output-to-string (thunk body ...)))

(define (compare a b)
  (cond [(< a b) 'less]
        [(> a b) 'greater]
        [else 'equal]))

(define (call/comparison comparison
                         #:less [less-thunk void]
                         #:greater [greater-thunk void]
                         #:equal [equal-thunk void])
  (case comparison
    [(less) (less-thunk)]
    [(greater) (greater-thunk)]
    [(equal) (equal-thunk)]))

(module+ test
  (define test-stx
    #'(require foo
         bar
                 (baz   blah
                    )
               bork   fork))
  (check-equal? (syntax->string test-stx)
                "(require foo\n   bar\n           (baz   blah)\n\n         bork   fork)"))
