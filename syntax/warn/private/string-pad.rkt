#lang typed/racket/base

(provide string-pad-right)

(require racket/string)

(module+ test
  (require typed/rackunit))


(: string-pad-right (-> String Char Nonnegative-Integer String))
(define (string-pad-right str pad-char min-length)
  (: num-below-minimum Nonnegative-Integer)
  (define num-below-minimum
    (max (- min-length (string-length str)) 0))
  (string-append str (make-string num-below-minimum pad-char)))

(module+ test
  (check-equal? (string-pad-right "foo" #\a 6) "fooaaa")
  (check-equal? (string-pad-right "foo" #\a 3) "foo")
  (check-equal? (string-pad-right "foo" #\a 1) "foo"))
