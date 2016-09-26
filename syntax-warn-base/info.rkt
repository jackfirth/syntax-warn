#lang info
(define name "syntax-warn")
(define version "0.1")
(define collection "syntax")
(define deps
  '(("base" #:version "6.4")))
(define build-deps
  '("rackunit-lib"))
(define compile-omit-paths
  '("warn/private"))
