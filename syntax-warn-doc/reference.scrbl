#lang scribble/manual

@(require "base.rkt")

@title[#:tag "syntax-warn-reference"]{The Syntax Warnings Reference}
@defmodule[syntax/warn #:packages ("syntax-warn" "syntax-warn-base")]

This module defines the programmatic API for adding @warn-tech{syntax
 warnings} to syntax objects. Warnings added via this library can be
detected and manipulated by the tools outlined in @secref{syntax-warn-cli}.

@defproc[(syntax-warning [#:message message string?]
                         [#:stx stx syntax?]
                         [#:kind kind warning-kind? #f]
                         [#:fix fix syntax? #f])
         syntax-warning?]{
 Constructs a @warn-tech{syntax warning} of kind @racket[kind] that identifies
 @racket[stx] as the syntax to blame and @racket[fix] as a suggested replacement
 to use in place of @racket[stx]. If @racket[kind] is not provided, the warning
 doesn't declare itself as having a particular kind. If @racket[fix] is not
 provided, the warning makes no suggestions about how to resolve it.}

@defproc[(syntax-warning? [v any/c]) boolean?]{
 Predicate that recognizes @warn-tech{syntax warnings}.}

@defproc[(syntax-warning/fix? [v any/c]) boolean?]{
 Predicate that recognizes @warn-tech{syntax warnings} that include a suggested
 fix.}

@deftogether[
 (@defproc[(syntax-warning-message [warning syntax-warning?]) string?]
   @defproc[(syntax-warning-stx [warning syntax-warning?]) syntax?]
   @defproc[(syntax-warning-kind [warning syntax-warning?]) (or/c warning-kind? #f)]
   @defproc[(syntax-warning-fix [warning syntax-warning?]) (or/c syntax? #f)])]{
 Accessors for fields of @warn-tech{syntax warnings}.}

@defthing[warning-kind? predicate/c]{
 Predicate recognizing warning kinds.}

@defform[(define-warning-kind id)]{
 Binds @racket[id] as a @kind-tech-definition{warning kind}, a value that
 can be used to classify similar warnings. The warning kind has a @italic{name},
 which is the quoted form of @racket[id]. The @exec{raco warn} tool uses
 warning kind names in its output.}

@defproc[(warning-kind-name [kind warning-kind?]) symbol?]{
 Returns the name of @racket[kind].}

@defproc[(syntax-warn [stx syntax?] [warning syntax-warning?]) syntax?]{
 Returns a syntax object equivalent to @racket[stx], but with @racket[warning]
 attached as a @warn-tech{syntax warning}. The syntax warning need not blame
 @racket[stx] as the source of the problem, this procedure merely provides the
 ability to attach warnings to syntax objects via syntax properties.
 @warn-examples[
 (define-warning-kind identifier-capitalization-warning)
 (syntax-warn #'(foo Bar)
              (syntax-warning #:message "Don't capitalize the \"Bar\" identifier"
                              #:stx #'foo
                              #:kind identifier-capitalization-warning))]}

@defproc[(syntax-warnings [stx syntax?]) (listof syntax?)]{
 Returns a list of all syntax warnings present in @racket[stx]. This includes
 syntax warnings in any syntax objects nested within @racket[stx].}

@defproc[(read-syntax-warnings [#:input-port in input-port? (current-input-port)]
                               [#:source-name source any/c (object-name in)]
                               [#:namespace namespace namespace? (current-namespace)])
         (listof syntax-warning?)]{
 Constructs a syntax object from @racket[in] using @racket[read-syntax], fully
 expands it using @racket[expand-syntax] in @racket[namespace], and returns a
 list of all syntax warnings found in the fully expanded module.}

@defproc[(read-syntax-warnings/file [filepath path-string?]
                                    [#:namespace namespace namespace? (current-namespace)])
         (listof syntax-warning?)]{
 Like @racket[read-syntax-warnings], but reads @racket[filepath] as a module.
 Sets the @racket[current-directory] to the directory part of @racket[filepath]
 and uses @racket[filepath] as the source name.}
