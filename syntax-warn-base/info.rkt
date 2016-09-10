#lang info
(define name "syntax-warn")
(define collection "syntax")
(define deps
  '("rackunit-lib"
    "typed-racket-lib"
    ("base" #:version "6.4")))
(define build-deps '("typed-racket-more"))
