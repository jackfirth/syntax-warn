#lang typed/racket/base

(provide string-append-lines)

(require racket/list)

(module+ test
  (require typed/rackunit))


(: string-append-lines (->* () #:rest String String))
(define (string-append-lines . strings)
  (apply string-append
         (add-between strings "\n")))

(module+ test
  (check-equal? (string-append-lines "foo" "bar" "baz")
                "foo\nbar\nbaz")
  (check-equal? (string-append-lines "foo") "foo")
  (check-equal? (string-append-lines "foo" "" "bar")
                "foo\n\nbar")
  (check-equal? (string-append-lines) ""))
