#lang scribble/manual

@(require "base.rkt")

@title[#:tag "syntax-warn-cli"]{The Syntax Warning Command Line Interface}

This document describes the @exec{raco warn} and @exec{raco fix}
commands. This commands allow a programmer to check for
@warn-tech{syntax warnings} and, where possible, automatically fix
them. The two commands are designed to work together - when
@exec{raco warn} outputs no warnings, @exec{raco fix} makes no
changes to any modules. Additionally, both commands accept the same
flags for specifying which modules to check.

@section{@exec{raco warn}: Checking Syntax Warnings}

The @exec{raco warn} command searches for @warn-tech{syntax warnings}
in a specified set of modules. Any found warnings are displayed with
a message, the offending source code, and a suggested fix (if present).
If any warnings are found the command exits as a failure, making it
suitable for use in continuous integration systems.

Not all warnings need cause a failure. The @exec{raco warn} command
allows certain warnings to be @italic{suppressed} by configuration.
For every module that @exec{raco warn} examines, the command looks
for a @config-tech{warning configuration} value named @racket[config]
provided by that module's @racket['warning-config] submodule. If this
module or the expected binding isn't present, @racket[empty-warning-config]
is used. This allows for per-module suppression of particular kinds
of warnings, see the documentation of @racket[warning-config] for
details.

The @exec{raco warn} command accepts any number of arguments along with
the following flags:

@itemize[
 @item{@DFlag{arg-kind} --- Sets how to interpret the given arguments.
  Defaults to "file". Valid interpretation modes are:
  @itemize[@item{file: Each argument is interpreted as a relative
              or absolute file path to a module.}
           @item{directory: Each argument is interpreted as a
              relative or absolute directory path, which is
              recursively scanned for modules. All files in
              the given directories are assumed to be modules.}
           @item{collection: Each argument is interpreted as a
              collection, whose modules are checked recursively.}
           @item{package: Each argument is interpreted as a
              package, whose modules are checked recursively.}]}
 @item{@Flag{f} or @DFlag{file} --- Shorthand for @exec{--arg-kind file}.}
 @item{@Flag{d} or @DFlag{directory} --- Shorthand for @exec{--arg-kind
   directory}.}
 @item{@Flag{c} or @DFlag{collection} --- Shorthand for @exec{--arg-kind
   collection}.}
 @item{@Flag{p} or @DFlag{package} --- Shorthand for @exec{--arg-kind
   package}.}
 @item{@Flag{config-submod} --- Sets the name of the submodule to
  look for warning configuration in. Required prior to loading of
  the surrounding module to check. Defaults to @racket['warning-config].}
 @item{@Flag{config-submod-binding} --- Sets the name of the value
  to look for in the warning configuration submodule. Defaults to
  @racket[config]. The warning configuration submodule should
  @racket[provide] a @racket[warning-configuration?] value under this
  name.}]

@section{@exec{raco fix}: Fixing Syntax Warnings}

The @exec{raco fix} command searches for @warn-tech{syntax warnings}
in a specified set of modules and fixes them, if possible. For each
module checked, the set of warnings is filtered to only warnings with
suggested fixes that won't interfere with each other. For instance, if
two warnings suggest changing the same piece of code, @exec{raco fix}
will either fix one of the warnings if its affected source code fully
encompasses the other warning's source code, or fix neither warning if
they only partially overlap. The @exec{raco fix} command also accepts
a @italic{run mode} argument that can configure how @exec{raco fix}
applies changes, if at all.

The @exec{raco fix} command accepts any number of arguments along with
the following flags:

@itemize[
 @item{@DFlag{arg-kind} --- Sets how to interpret the given arguments.
  This flag accepts the same values and has the same default as it does
  for @exec{raco warn}. Additionally, the same shorthand flags for the
  various values are accepted.}
 @item{@Flag{D} or @DFlag{dry-run} --- Sets the run mode to @italic{
   dry run}. In a dry run, @exec{raco fix} performs no file writes and
  merely outputs what it would fix in which modules.}]

In addition, the @exec{raco fix} command looks for @config-tech{warning
 configuration} in the same way as @exec{raco warn} with the same flags
to control this behavior.
