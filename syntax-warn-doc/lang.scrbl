#lang scribble/manual

@(require "base.rkt")

@title[#:tag "syntax-warn-lang"]{The Syntax Warnings Language}
@defmodule[racket/base/warn #:packages ("syntax-warn" "syntax-warn-lang")]

This library provides a variant of the @racketmodname[racket/base]
language that adds @warn-tech{syntax warning} wrappers around various
forms. For example, the @racket[require] binding introduced by this
language enforces a few "good style" practices around require clauses.

When @racketmodname[racket/base/warn] is used as a @hash-lang[] language,
everything from @racketmodname[racket/base] is provided. Various forms
are adjusted to attach @warn-tech{syntax warnings} in certain cases.
@kind-tech{Warning kinds} for these warnings are also exported.

@defform[(require require-spec ...)]{
 Like in @racketmodname[racket/base], but warns @racket[require:phase-order]
 when the given @racket[require-spec] forms are not in
 @phase-order-tech-definition{phase order}. Phase order is @racket[for-syntax]
 first, @racket[for-template] second, @racket[for-label] third,
 @racket[for-meta] fourth, and all other forms fifth.}

@defthing[require:phase-order warning-kind?]{
 A @kind-tech{warning kind} for when @racket[require] clauses are not
 @phase-order-tech{phase ordered}.}
