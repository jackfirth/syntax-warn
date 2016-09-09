#lang racket/base

(require racket/cmdline
         racket/function
         racket/list
         racket/match
         raco/command-name
         syntax/modread
         "main.rkt"
         "private/module.rkt"
         "private/string-lines.rkt"
         "private/syntax-srcloc.rkt"
         "private/syntax-string.rkt")

(module+ test
  (require racket/port
           rackunit
           "private/rackunit-string.rkt"))


(define (srcloc-location-string srcloc)
  (format "L~a:C~a:"
          (srcloc-line srcloc)
          (srcloc-column srcloc)))

(module+ test
  (check-equal? (srcloc-location-string (make-srcloc "foo" 1 2 3 4))
                "L1:C2:"))

(define (separator-format sep width)
  (define separator (make-string width sep))
  (string-append-lines separator "~a" separator ""))

(module+ test
  (check-equal? (separator-format #\X 4)
                "XXXX\n~a\nXXXX\n"))

(define (format-warning warning #:separator-char [sep #\-] #:separator-width [width 80])
  (define stx (syntax-warning-stx warning))
  (define fix (syntax-warning-fix warning))
  (define message (syntax-warning-message warning))
  (define warning-message
    (format "~a ~a"
            (srcloc-location-string (syntax-srcloc stx))
            message))
  (define message-format
    (if fix
        (separator-format sep width)
        "~a\n"))
  (define (indent str) (string-append "  " str))
  (format message-format
          (if fix
              (string-append-lines
               warning-message
               ""
               (syntax->string/line-numbers stx #:indent-spaces 3)
               ""
               "suggested fix:"
               ""
               (syntax->string/line-numbers fix #:indent-spaces 3))
              warning-message)))

(module+ test
  (define-warning-kind raco-test-kind)
  (test-case "Formatted warning without a suggested fix"
    (define formatted-warning
      (format-warning
       (syntax-warning #:message "not there"
                       #:kind raco-test-kind
                       #:stx #'here)))
    (check-string-contains? formatted-warning "not there")
    (check-string-has-trailing-newline? formatted-warning))
  (test-case "Formatted warning with a suggested fix"
    (define test-stx #'foo)
    (define test-stx/fix
      (datum->syntax test-stx 'bar test-stx test-stx))
    (define warning/fix
      (syntax-warning #:message "use a different name"
                      #:kind raco-test-kind
                      #:stx test-stx
                      #:fix test-stx/fix))
    (define expected-message-strings
      (list "----------------"
            "L" "C"
            "use a different name"
            "foo"
            "suggested fix:"
            "bar"))
    (check-string-contains-all? (format-warning warning/fix)
                                expected-message-strings)))

(define (print-warning warning)
  (printf (format-warning warning))
  (flush-output))

(define (parse-warn-command!)
  (define kind-param (make-parameter 'file))
  (command-line
   #:program (short-program+command-name)
   #:once-any
   ["--arg-kind" kind
                 ("How to interpret the arguments as modules"
                  "One of file, directory, collection, or package"
                  "Defaults to file")
                 (define kind-sym (string->symbol kind))
                 (check-kind! kind-sym)
                 (kind-param kind-sym)]
   [("-f" "--file-args") ("Interpret the arguments as files"
                          "Files are required as modules and checked"
                          "Equivalent to \"--arg-kind file\", default behavior")
                         (kind-param 'file)]
   [("-d" "--directory-args") ("Interpret the arguments as directories"
                               "Modules in directories are recursively checked"
                               "Equivalent to \"--arg-kind directory\"")
                              (kind-param 'directory)]
   [("-c" "--collection-args") ("Interpet the arguments as collections"
                                "Modules in collections are recursively checked"
                                "Equivalent to \"--arg-kind collection\"")
                               (kind-param 'collection)]
   [("-p" "--package-args") ("Interpret the arguments as packages"
                             "Modules in packages are recursively checked"
                             "Equivalent to \"--arg-kind package\"")
                            (kind-param 'package)]
   #:args (arg . args)
   (module-args (kind-param) (cons arg args))))

(define (check-kind! k)
  (unless (member k (list 'file 'directory 'collection 'package))
    (raise-arguments-error
     (string->symbol (short-program+command-name))
     "expected an arg kind of file, directory, collection, or package"
     "--arg-kind" k)))

(module+ test
  (test-case "parse-warn-command!"
    (define (parse/args args)
      (parameterize ([current-command-line-arguments args])
        (parse-warn-command!)))
    (check-equal? (parse/args (vector "-f" "foo" "bar"))
                  (module-args 'file (list "foo" "bar")))
    (check-equal? (parse/args (vector "-d" "foo" "bar"))
                  (module-args 'directory (list "foo" "bar")))
    (check-equal? (parse/args (vector "-c" "foo" "bar"))
                  (module-args 'collection (list "foo" "bar")))
    (check-equal? (parse/args (vector "-p" "foo" "bar"))
                  (module-args 'package (list "foo" "bar")))
    (check-equal? (parse/args (vector "--arg-kind" "file" "foo" "bar"))
                  (module-args 'file (list "foo" "bar")))))

(define (warn-modules resolved-module-paths)
  (define any-warned? (box #f))
  (define warnings-namespace (make-base-namespace))
  (for ([modpath resolved-module-paths])
    (printf "raco warn: ~a\n" modpath)
    (flush-output)
    (define warnings
      (read-syntax-warnings/file modpath #:namespace warnings-namespace))
    (for ([warning warnings])
      (set-box! any-warned? #t)
      (print-warning warning)))
  (unbox any-warned?))

(define (run-warn-command! module-args)
  (define modules (module-args->modules module-args))
  (match (length modules)
    [(== 0) (printf "No modules found\n")]
    [(== 1) (printf "Checking 1 module\n")]
    [num-modules (printf "Checking ~a modules\n" num-modules)])
  (flush-output)
  (if (warn-modules modules) 1 0))

(module+ test
  (test-case "run-warn-command!"
    (define (test-command args)
      (define output (open-output-string))
      (define code
        (parameterize ([current-output-port output])
          (run-warn-command! args)))
      (list code (get-output-string output)))
    (define no-warn-result
      (test-command (module-args 'collection
                                 (list "warn/test-no-warnings"))))
    (define warn-result
      (test-command (module-args 'collection
                                 (list "warn/test-warnings"))))
    (define no-warn-expected-strs
      (list "Checking"
            "module"
            "raco warn: "
            "test-no-warnings/main.rkt"))
    (check-string-contains-all? (second no-warn-result)
                                no-warn-expected-strs)
    (define warn-expected-strs
      (list "Checking"
            "module"
            "raco warn: "
            "test-warnings/main.rkt"
            "---------"
            "phase order"))
    (check-string-contains-all? (second warn-result)
                                warn-expected-strs)
    (check-equal? (first no-warn-result) 0)
    (check-equal? (first warn-result) 1)))

(module+ main
  (exit (run-warn-command! (parse-warn-command!))))
