#lang racket

(require racket/runtime-path
         rackunit
         syntax/srcloc
         "syntax-srcloc.rkt")

(define test-stx
  #'(require (for-syntax foo
      bar
                 baz)
             
          blah
                  (bang
                   
                foo)))

(define test-stx-start (source-location-position test-stx))
(define test-stx-line (source-location-line test-stx))
(define test-stx-span (source-location-span test-stx))

(define-runtime-path here "syntax-srcloc-test.rkt")

(check-equal?
 (syntax-complete-srcloc test-stx)
 (complete-srcloc #:source here
                  #:position test-stx-start
                  #:position-span test-stx-span
                  #:line test-stx-line
                  #:line-span 7
                  #:column 4
                  #:column-end 21))

(check-equal?
 (syntax-complete-srcloc (first (syntax->list test-stx)))
 (complete-srcloc #:source here
                  #:position (add1 test-stx-start)
                  #:position-span 7
                  #:line test-stx-line
                  #:line-span 0
                  #:column 5
                  #:column-end 12))

(check-equal?
 (syntax-complete-srcloc (second (syntax->list test-stx)))
 (complete-srcloc #:source here
                  #:position (+ test-stx-start 9)
                  #:position-span 47
                  #:line test-stx-line
                  #:line-span 2
                  #:column 13
                  #:column-end 21))
