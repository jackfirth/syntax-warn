#lang racket/base

(provide (for-label (all-from-out racket/base
                                  racket/contract
                                  syntax/warn))
         config-tech
         config-tech-definition
         document-syntax-parameter
         kind-tech
         kind-tech-definition
         phase-order-tech
         phase-order-tech-definition
         source-code-link
         warn-examples
         warn-tech
         warn-tech-definition)

(require (for-label racket/base
                    racket/contract
                    syntax/warn)
         scribble/base
         scribble/example
         scribble/manual
         scribble/text
         syntax/parse/define)


(define-simple-macro
  (define-techs [key:str use-id:id def-id:id] ...)
  (begin
    (begin
      (define (def-id . pre-flow)
        (apply deftech #:key key pre-flow))
      (define (use-id . pre-flow)
        (apply tech #:key key #:doc warn-doc pre-flow)))
    ...))

(define warn-doc
  '(lib "syntax/warn/main.scrbl"))

(define-techs
  ["phase order" phase-order-tech phase-order-tech-definition]
  ["syntax warning" warn-tech warn-tech-definition]
  ["warning config" config-tech config-tech-definition]
  ["warning kind" kind-tech kind-tech-definition])

(define (source-code-link url-str)
  (begin/text "Source code for this library is avaible at "
              (url url-str)))

(define warn-requires
  '(syntax/warn))

(define (make-warn-eval)
  (make-base-eval #:lang 'racket/base
                  (cons 'require warn-requires)))

(define (make-warn-lang-eval)
  (make-base-eval #:lang 'racket/base/warn))

(define-syntax-rule (warn-examples example ...)
  (examples #:eval (make-warn-eval) example ...))

(define-syntax-rule (document-syntax-parameter id pre-flow ...)
  (defform #:kind "syntax parameter" #:id id id pre-flow ...))
