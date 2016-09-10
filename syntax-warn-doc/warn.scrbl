#lang scribble/manual

@(require (for-label syntax/warn
                     racket/base
                     racket/contract
                     racket/function)
          scribble/examples)

@(define (warn-tech . pre-content)
   (apply tech #:key "syntax-warning" #:normalize? #f pre-content))

@title{Racket with Syntax Warnings}

This library provides a variant of the @racketmodname[racket/base]
language that adds @warn-tech{syntax warning} wrappers around various
forms. For example, the @racket[require] binding introduced by this
language enforces a few "good style" practices around require clauses.
Additionally, modules are provided to manually add the same warnings
produced by this language, in case other languages wish to use the
same style rules.
