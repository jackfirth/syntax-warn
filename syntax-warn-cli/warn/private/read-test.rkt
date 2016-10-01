#lang racket/base

;; This module is read as input for testing in another module

(require (for-syntax racket/base
                     syntax/warn))

(begin-for-syntax
  (define-warning-kind module-test-kind))

(define-syntax (add-warnings stx)
  (define stx/warning
    (syntax-warn stx
                 (syntax-warning #:message "Test warning"
                                 #:kind module-test-kind
                                 #:stx (syntax-local-introduce stx))))
  (datum->syntax stx/warning '(void) stx/warning stx/warning))

(add-warnings)