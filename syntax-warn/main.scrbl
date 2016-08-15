#lang scribble/manual

@(require scribble/examples (for-label warn racket/base racket/stxparam racket/contract racket/function))

@(define (warn-tech . pre-content)
   (apply tech #:key "syntax-warning" #:normalize? #f pre-content))
@(define-syntax-rule (document-syntax-property-key id pre-flow ...)
   (defform #:kind "syntax property key" #:id id id pre-flow ...))
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

@defstruct*[syntax-warning
            ([location srcloc?] [message string?] [fix suggested-fix?])
            #:transparent]{
 Structure representing a @warn-tech{syntax warning}. Syntax warnings contain a
 source location identifying the source of the warning and a human-readable message
 describing the warning.
}

@defstruct*[suggested-fix
            ([original-stx syntax?] [replacement-stx syntax?])
            #:transparent]{
 Structure representing a way to fix a @warn-tech{syntax warning}. By replacing
 the content at @racket[original-stx]'s source location with the content of
 @racket[replacement-stx], the warning this fix was attached to will be resolved.
}

@section{Attaching warnings to syntax}

@defproc[(syntax-warn [stx syntax?]
                      [message string?]
                      [#:fix replacement-stx (or/c syntax? #f) #f]
                      [#:bad-stx bad-stx syntax? stx])
         syntax?]{
 Returns a syntax object equivalent to @racket[stx] with a @warn-tech{syntax warning}
 attached. The warning points to @racket[bad-stx] as its source, which defaults to
 @racket[stx] if not provided. The warning has @racket[message] as its message.
 If @racket[replacement-stx] is not @racket[#f], the warning suggests replacing
 @racket[bad-stx] with @racket[replacement-stx]. The warning is added under the
 @racket[syntax-warning-property-key] syntax property.
 @syntax-warn-examples[
 (syntax-warn #'(lambda (lambda) lambda)
              "Shadowing the language defined identifier \"lambda\" is discouraged")
 ]}

@document-syntax-property-key[syntax-warnings-property-key]{
 A value used as the @racket[syntax-property] key that @warn-tech{syntax-warnings}
 are attached to. This value is not guaranteed to remain constant across package
 versions.
 @syntax-warn-examples[syntax-warnings-property-key]}

@defproc[(syntax-warnings [stx syntax?]) (listof syntax?)]{
 Returns a list of all syntax warnings present in @racket[stx]. This includes
 syntax warnings in any syntax objects nested within @racket[stx].
 @syntax-warn-examples[
 (syntax-warnings
  (syntax-warn
   (syntax-warn #'(lambda (lambda) lambda)
                "Shadowing the language defined identifier \"lambda\" is discouraged")
   "This function is identicial to the built in \"identity\" procedure"
   #:fix #'identity))
 ]}

@section{Checking warnings}

@defproc[(check-syntax-warnings [stx syntax?]) Void]{
 Retrieves all @warn-tech{syntax warnings} associated with @racket[stx] and logs
 them to @racket[current-error-port].
 @syntax-warn-examples[
 (check-syntax-warnings
  (syntax-warn
   (syntax-warn #'(lambda (lambda) lambda)
                "Shadowing the language defined identifier \"lambda\" is discouraged")
   "This function is identicial to the built in \"identity\" procedure"
   #:fix #'identity))
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

@section{Fixing warnings}

@defproc[(fix-warning! [fix suggested-fix?]) void?]{
 Applies @racket[fix] by locating the file that the fix's origin syntax source
 location points to and replacing that syntax's contents with the suggested fix.
 Source locations in the suggested replacement syntax are used to determine
 formatting.
}
