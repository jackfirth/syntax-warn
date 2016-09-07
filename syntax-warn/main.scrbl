#lang scribble/manual

@(require scribble/examples (for-label warn racket/base racket/stxparam racket/contract racket/function))

@(define (warn-tech . pre-content)
   (apply tech #:key "syntax-warning" #:normalize? #f pre-content))
@(define-syntax-rule (syntax-warn-examples example ...)
   (examples #:eval (make-base-eval #:lang 'racket/base '(require warn)) example ...))
@(define-syntax-rule (document-syntax-parameter id pre-flow ...)
   (defform #:kind "syntax parameter" #:id id id pre-flow ...))

@title{Syntax Warnings}
@defmodule[warn]
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

This library provides forms for working with @warn-tech{syntax warnings}. Library
and language developers can make deprecation notices, optimizations, simplifications,
and style rules visible to users, and users can suppress warnings and warning
categories in both individual code snippets and package-wide configuration.

@section{Warnings}

A @deftech[#:key "syntax-warning" #:normalize? #f]{syntax warning} is an expansion
time annotation that draws attention to a potential defect in a program's syntax.
Such defects are not technically incorrect behavior, but signal a likely problem.
Examples include using a deprecated library feature, naming variables in a way
that's considered bad style, using a particular language feature in a way known to
cause subtle bugs, and so on. Syntax warnings are first class values attached to
source code at compile time through syntax properties, and can be inspected by
other code and tools.

@defstruct*[warning-kind ([name symbol?]) #:prefab]{
 Structure representing a warning kind. @warn-tech{Syntax warnings} often have
 similar sources and causes, and it can be helpful to group them under a warning
 kind. The @racket[name] of the warning kind is used for reporting.}

@defproc[(syntax-warning? [v any/c]) boolean?]{
 Predicate that recognizes @warn-tech{syntax warnings}.}

@defproc[(syntax-warning [#:message message string?]
                         [#:kind kind warning-kind?]
                         [#:stx stx syntax?]
                         [#:fix fix syntax? #f])
         syntax-warning?]{
 Constructs a @warn-tech{syntax warning} of kind @racket[kind] that identifies
 @racket[stx] as the syntax to blame and @racket[fix] as a suggested replacement
 to use in place of @racket[stx]. If @racket[fix] is not provided, the warning
 makes no suggestions about how to resolve it.}

@section{Attaching warnings to syntax}

@defproc[(syntax-warn [stx syntax?]
                      [warning syntax-warning?])
         syntax?]{
 Returns a syntax object equivalent to @racket[stx], but with @racket[warning]
 attached as a @warn-tech{syntax warning}. The syntax warning need not blame
 @racket[stx] as the source of the problem, this procedure merely provides the
 ability to attach warnings to syntax objects via syntax properties.
 @syntax-warn-examples[
 (syntax-warn #'(foo Bar)
              (syntax-warning #:message "Don't capitalize the \"Bar\" identifier"
                              #:kind (warning-kind 'identifier-capitalization)
                              #:stx #'foo))]}

@defproc[(syntax-warnings [stx syntax?]) (listof syntax?)]{
 Returns a list of all syntax warnings present in @racket[stx]. This includes
 syntax warnings in any syntax objects nested within @racket[stx].}
