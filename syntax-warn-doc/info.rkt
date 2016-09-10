#lang info
(define name "syntax-warn-doc")
(define scribblings
  '(("main.scrbl" () (library) "syntax-warn")))
(define deps '("base"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "syntax-warn-base"))
