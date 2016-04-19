#lang scribble/manual

@(require syntax/warn scribble/examples (for-label typed/racket/base))

@(define (warn-tech . pre-content)
  (apply tech #:key "syntax-warning" #:normalize? #f pre-content))
@(define-syntax-rule (document-type id pre-flow ...)
   (defform #:kind "type" #:id id id pre-flow ...))
@(define-syntax-rule (document-syntax-property-key id pre-flow ...)
   (defform #:kind "syntax property key" #:id id id pre-flow ...))
@(define-syntax-rule (syntax-warn-examples example ...)
   (examples #:eval (make-base-eval #:lang 'typed/racket/base '(require syntax/warn)) example ...))
@(define-syntax-rule (document-syntax-parameter id pre-flow ...)
   (defform #:kind "syntax parameter" #:id id id pre-flow ...))

@title{Syntax Warnings}
@defmodule[syntax/warn]
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

@deftogether[
  (@document-type[Syntax-Warning]
   @defstruct*[syntax-warning ([location Srcloc] [message String]) #:transparent])]{
  Structure representing a syntax warning. Syntax warnings contain a source location
  identifying the source of the warning and a human-readable message describing the
  warning.
}

@document-type[Srcloc]{The type of instances of the @racket[srcloc] structure}

@section{Attaching warnings to syntax}

@defproc[(syntax-warn [stx Syntax] [message String]) Syntax]{
  Returns a syntax object equivalent to @racket[stx] with a @racket[syntax-warning]
  attached. The warning points to @racket[stx] as its source and has @racket[message]
  as its message. The warning is added under the @racket[syntax-warning-property-key]
  syntax property.
  @syntax-warn-examples[
    (syntax-warn #'(lambda (lambda) lambda)
                 "Shadowing the language defined identifier \"lambda\" is discouraged")
]}

@document-syntax-property-key[syntax-warnings-property-key]{
  A value used as the @racket[syntax-property] key that @racket[syntax-warning]s
  are attached to. This value is not guaranteed to remain constant across package
  versions.
  @syntax-warn-examples[syntax-warnings-property-key]}

@defproc[(syntax-warnings [stx Syntax]) (Listof Syntax-Warning)]{
  Returns a list of all syntax warnings present in @racket[stx]. This includes
  syntax warnings in any syntax objects nested within @racket[stx].
  @syntax-warn-examples[
    (syntax-warnings
     (syntax-warn
      (syntax-warn #'(lambda (lambda) lambda)
                   "Shadowing the language defined identifier \"lambda\" is discouraged")
      "This function is identicial to the built in \"identity\" procedure"))
]}

@section{Checking warnings}

@defproc[(check-syntax-warnings [stx Syntax]) Void]{
  Retrieves all @warn-tech{syntax warnings} associated with @racket[stx] and logs
  them to @racket[current-error-port].
  @syntax-warn-examples[
    (check-syntax-warnings
     (syntax-warn #'(lambda (lambda) lambda)
                  "Shadowing the language defined identifier \"lambda\" is discouraged"))
]}

@document-syntax-parameter[warned-module-begin]{
  A @tech{syntax parameter} defining the base @racket[#%module-begin] form used by
  @racket[module-begin/warn]. This allows arbitrary languages to be extended with
  syntax warnings.
}

@defform[(module-begin/warn body ...)]{
  Equivalent to @racket[#%module-begin], but calls @racket[check-syntax-warnings]
  during expansion. The base @racket[#%module-begin] form used can be changed via
  the @racket[warned-module-begin] syntax parameter.
}
