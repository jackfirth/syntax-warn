#lang racket/base/warn

(require rackunit
         (for-syntax syntax/parse
                     syntax/parse/define)
         racket/format)

(module warning-config racket/base
  (require syntax/warn
           syntax/warn/private/warn-config
           racket/base/warn)
  (provide config)
  (define config (suppress require:phase-order)))
