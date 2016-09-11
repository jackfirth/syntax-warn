#lang scribble/manual

@(require "base.rkt")

@title[#:tag "syntax-warn-lang"]{The Syntax Warnings Language}
@defmodule[racket/base/warn #:packages ("syntax-warn" "syntax-warn-lang")]

This library provides a variant of the @racketmodname[racket/base]
language that adds @warn-tech{syntax warning} wrappers around various
forms. For example, the @racket[require] binding introduced by this
language enforces a few "good style" practices around require clauses.

@section{Warnings as a @hash-lang[]}

When @racketmodname[racket/base/warn] is used as a @hash-lang[] language,
everything from @racketmodname[racket/base] is provided. Various forms
are adjusted to attach @warn-tech{syntax warnings} in certain cases.
These adjustments are documented here.

@defform[(require require-spec ...)]{
 Like in @racketmodname[racket/base], but warns @racket[require:phase-order]
 when the given @racket[require-spec] forms are not in
 @phase-order-tech-definition{phase order}. Phase order is @racket[for-syntax]
 first, @racket[for-template] second, @racket[for-label] third,
 @racket[for-meta] fourth, and all other forms fifth.}

@section{Warnings as a module}

When @racketmodname[racket/base/warn] is @racket[require]'d as a module,
it provides warning kinds and functions for adding warnings that @hash-lang[]
@racketmodname[racket/base/warn] uses. These are documented here.

@defthing[require:phase-order warning-kind?]{
 A @kind-tech{warning kind} for when @racket[require] clauses are not
 @phase-order-tech{phase ordered}.}

@defproc[(syntax-warn/require-phase-order [require-syntax syntax?]) syntax?]{
 Examines @racket[require-syntax], which is expected to be a
 @racket[(require clause ...)] syntax object, and returns it with warnings
 attached in the same way as @racket[require] does in @hash-lang[]
 @racketmodname[racket/base/warn].}
