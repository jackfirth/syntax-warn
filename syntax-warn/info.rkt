#lang info
(define name "syntax-warn")
(define collection "warn")
(define deps
  '(("base" #:version "6.4")))
(define scribblings '(("main.scrbl" () (library) "warn")))
(define raco-commands
  '(("warn" (submod warn/raco-warn main) "Check for syntax warnings" 20)))
