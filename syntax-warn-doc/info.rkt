#lang info
(define name "syntax-warn-doc")
(define scribblings
  '(("main.scrbl" (multi-page) (library) "syntax-warn")))
(define deps '("syntax-warn-base"
               "scribble-lib"
               "scribble-text-lib"
               "base"))
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "syntax-warn-base"))
