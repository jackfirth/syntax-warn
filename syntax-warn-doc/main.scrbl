#lang scribble/manual

@(require "base.rkt")

@title[#:style '(toc)]{Syntax Warnings}
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

This library includes forms and functions for working with syntax warnings.
@warn-tech-definition{A syntax warning} is a compile-time source code annotation
meant to draw attention to potential defects. Syntax warnings can be used to
point out bad code style, sub-optimal constructs, excessively verbose code, or
use of deprecated features. Additionally included in this library are the
@exec{raco warn} and @exec{raco fix} commands for finding and fixing warnings,
and the @hash-lang[] @racketmodname[racket/base/warn] language which provides
many forms in @racketmodname[racket/base] with warnings attached in various
ways.

@source-code-link{https://github.com/jackfirth/syntax-warn}

This library is provided as several packages. These packages are organized as
follows:

@itemize[
 @item{@package-name{syntax-warn-base} --- The core API of syntax warnings, as
  defined by the @racketmodname[syntax/warn] module.}
 @item{@package-name{syntax-warn-cli} --- The @exec{raco} command line tools for
  working with warnings.}
 @item{@package-name{syntax-warn-lang} --- The @racketmodname[racket/base/warn]
  language.}
 @item{@package-name{syntax-warn-doc} --- The documentation part of the
  library.}
 @item{@package-name{syntax-warn} --- A wrapper package that implies
  installation of all of the above packages.}]

@table-of-contents[]
@include-section["reference.scrbl"]
@include-section["cli.scrbl"]
@include-section["lang.scrbl"]
