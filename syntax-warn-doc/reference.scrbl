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
 ((defproc (syntax-warning-message [warning syntax-warning?]) string?)
  (defproc (syntax-warning-stx [warning syntax-warning?]) syntax?)
  (defproc (syntax-warning-kind [warning syntax-warning?]) (or/c warning-kind? #f))
  (defproc (syntax-warning-fix [warning syntax-warning?]) (or/c syntax? #f)))]{
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

@defproc[(warning-config? [v any/c]) boolean?]{
 Predicate that recognizes @config-tech-definition{warning configurations},
 values used to control the behavior of tools that analyze syntax warnings.
 Configurations from different souces may be merged with
 @racket[warning-config-merge]. Currently, warning configurations contain
 information about what kinds of warnings should be suppressed or unsuppressed
 (in the case of warnings that aren't on by default). See @racket[suppress]
 and @racket[unsuppress] for details on how to construct these configurations.}

@defproc[(warning-config-merge [config warning-config?] ...) warning-config?]{
 Merges the given @racket[config] values into a single @config-tech{warning
  configuration}. In the case of more than one @racket[config] attempting to
 suppress or unsuppress a warning kind, the rightmost configuration takes
 precedence. Merging any config with the @racket[empty-warning-config] is
 equivalent to leaving the config unchanged. If no configurations are provided,
 returns @racket[empty-warning-config].}

@defthing[empty-warning-config warning-config?]{
 The empty warning config. Represents no changes from default behavior.}

@deftogether[
 ((defproc (suppress [kind (or/c warning-kind? symbol?)] ...) warning-config?)
  (defproc (unsuppress [kind (or/c warning-kind? symbol?)] ...)
    warning-config?))]{
 Constructs a @config-tech{warning configuration} where warnings of each of the
 given @racket[kind]s is either suppressed or unsuppressed, depending on the
 function called. Warnings may be suppressed using an explicit warning kind
 value or suppressed by a symbol representing the name of the warning kind to
 suppress. Symbols allow configuration sources to construct warning kind configs
 without access to the warning kind bindings. To suppress some warnings and
 unsuppress others, see @racket[warning-config-merge].}

@defproc[(filter-unsuppressed-warnings [warnings (listof syntax-warning?)]
                                       [config warning-config?])
         (listof syntax-warning?)]{
 Returns all the @warn-tech{syntax warnings} in @racket[warnings] that are
 not suppressed according to @racket[config].}
