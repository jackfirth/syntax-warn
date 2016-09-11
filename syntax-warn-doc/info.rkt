#lang info
(define name "syntax-warn-doc")
(define scribblings
  '(("main.scrbl" () (library) "syntax-warn")))
(define deps '("scribble-lib"
               "scribble-text-lib"
               "base"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "syntax-warn-base"))
