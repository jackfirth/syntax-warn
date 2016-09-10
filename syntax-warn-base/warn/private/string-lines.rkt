#lang typed/racket/base

(provide string-append-lines
         string-indent-lines
         string->lines
         lines->string)

(require racket/list
         racket/string)

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

(: string-indent-lines (-> String Nonnegative-Integer String))
(define (string-indent-lines str n)
  (: indent-line (-> String String))
  (define (indent-line line)
    (string-append (make-string n #\space) line))
  (lines->string (map indent-line (string->lines str))))

(module+ test
  (check-equal? (string-indent-lines "foo\nbar" 2) "  foo\n  bar"))

(: string->lines (-> String (Listof String)))
(define (string->lines str)
  (if (equal? str "")
      (list "")
      (string-split str "\n" #:trim? #f)))

(module+ test
  (check-equal? (string->lines "foo\nbar") (list "foo" "bar"))
  (check-equal? (string->lines "\nfoo\n\nbar\n") (list "" "foo" "" "bar" ""))
  (check-equal? (string->lines "") (list ""))
  (check-equal? (string->lines "\n") (list "" "")))

(: lines->string (-> (Listof String) String))
(define (lines->string lines)
  (string-join lines "\n"))

(module+ test
  (check-equal? (lines->string (list "foo" "bar")) "foo\nbar")
  (check-equal? (lines->string (list "" "foo" "")) "\nfoo\n")
  (check-equal? (lines->string (list "foo  " "bar")) "foo  \nbar"))
