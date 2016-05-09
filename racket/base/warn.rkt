#lang racket/base

(module+ initial-bindings

  (require (for-syntax racket/base)
           racket/base
           racket/splicing
           racket/stxparam
           warn/module
           warn/require-syntax)

  (provide (except-out (all-from-out racket/base) #%module-begin require)
           (rename-out [module-begin/warn #%module-begin]
                       [require/warn/shadow require])))

(module reader syntax/module-reader
  #:language '(submod racket/base/warn initial-bindings))
