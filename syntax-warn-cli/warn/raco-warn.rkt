#lang racket/base

(require racket/cmdline
         racket/function
         racket/list
         racket/match
         racket/string
         raco/command-name
         syntax/modread
         syntax/warn
         syntax/warn/private/warn-config
         syntax/warn/private/string-lines
         syntax/warn/private/syntax-srcloc
         syntax/warn/private/syntax-string
         "private/module.rkt"
         "private/config.rkt")

(module+ test
  (require racket/port
           rackunit
           syntax/warn/private/rackunit-string))


(define (separator-format sep width)
  (define separator (make-string width sep))
  (string-append-lines separator "~a" separator ""))

(module+ test
  (check-equal? (separator-format #\X 4)
                "XXXX\n~a\nXXXX\n"))

(define (list/filter . vs)
  (filter values vs))

(define (format-warning warning)
  (define message (syntax-warning-message warning))
  (define stx (syntax-warning-stx warning))
  (define fix (syntax-warning-fix warning))
  (define kind (syntax-warning-kind warning))
  (define kind-prefix
    (and kind (format "[~a]" (warning-kind-name kind))))
  (define message-part
    (string-join (list/filter kind-prefix message)))
  (define warning-part
    (string-indent-lines (syntax->string/line-numbers stx) 2))
  (define fix-part/noindent
    (and fix
         (string-append "suggested fix:\n\n"
                        (syntax->string/line-numbers fix))))
  (define fix-part
    (and fix-part/noindent
         (string-indent-lines fix-part/noindent 2)))
  (string-join (list/filter message-part warning-part fix-part)
               "\n\n" #:after-last "\n"))

(module+ test
  (define-warning-kind raco-test-kind)
  (test-case "Formatted warning without a suggested fix"
    (define formatted-warning
      (format-warning
       (syntax-warning #:message "not there"
                       #:kind raco-test-kind
                       #:stx #'here)))
    (check-string-contains-all? formatted-warning
                                (list "not there"
                                      "here"
                                      "[raco-test-kind]"))
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
      (list "use a different name"
            "foo"
            "suggested fix:"
            "bar"))
    (check-string-contains-all? (format-warning warning/fix)
                                expected-message-strings)))

(define (print-warning warning)
  (printf (format-warning warning))
  (flush-output))

(struct warn-command-args
  (module-args config-submod-args)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-warn-command-args)

(define (warn-command-args #:module [mod #f]
                           #:config-submod [config-submod #f])
  (make-warn-command-args (or mod (module-args 'file '()))
                          (or config-submod (config-submod-args))))

(define (parse-warn-command!)
  (define kind-param (make-parameter 'file))
  (define config-submod-param (make-parameter #f))
  (define config-submod-binding-param (make-parameter #f))
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
   #:once-each
   ["--config-submod" submod
                      ("Name of the submodule to look for warning configuration in"
                       "Defaults to 'warning-config")
                      (config-submod-param (string->symbol submod))]
   ["--config-submod-binding" submod-binding
                              ("Name of the binding to require from configuration submodules"
                               "Defaults to 'config")
                              (config-submod-binding-param
                               (string->symbol submod-binding))]
   #:args (arg . args)
   (warn-command-args
    #:module (module-args (kind-param) (cons arg args))
    #:config-submod (config-submod-args
                     #:submod-name (config-submod-param)
                     #:binding-name (config-submod-binding-param)))))

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
                  (warn-command-args
                   #:module (module-args 'file (list "foo" "bar"))))
    (check-equal? (parse/args (vector "-d" "foo" "bar"))
                  (warn-command-args
                   #:module (module-args 'directory (list "foo" "bar"))))
    (check-equal? (parse/args (vector "-c" "foo" "bar"))
                  (warn-command-args
                   #:module (module-args 'collection (list "foo" "bar"))))
    (check-equal? (parse/args (vector "-p" "foo" "bar"))
                  (warn-command-args
                   #:module (module-args 'package (list "foo" "bar"))))
    (check-equal? (parse/args (vector "--arg-kind" "file" "foo" "bar"))
                  (warn-command-args
                   #:module (module-args 'file (list "foo" "bar"))))
    (check-equal? (parse/args (vector "--config-submod" "foo"
                                      "--config-submod-binding" "bar"
                                      "some/file"))
                  (warn-command-args
                   #:module (module-args 'file (list "some/file"))
                   #:config-submod (config-submod-args #:submod-name 'foo
                                                       #:binding-name 'bar)))))

(define (warn-modules resolved-module-paths config-submod-args)
  (define any-warned? (box #f))
  (define warnings-namespace (make-base-namespace))
  (for ([modpath resolved-module-paths])
    (printf "raco warn: ~a\n" modpath)
    (flush-output)
    (define all-warnings
      (read-syntax-warnings/file modpath #:namespace warnings-namespace))
    (define config
      (require-warning-config-submod modpath config-submod-args))
    (define warnings (filter-unsuppressed-warnings all-warnings config))
    (when (not (empty? warnings))
      (define warning-strs (map format-warning warnings))
      (write-string
       (string-join warning-strs "\n" #:before-first "\n" #:after-last "\n"))
      (set-box! any-warned? #t)))
  (unbox any-warned?))

(define (run-warn-command! warn-command-args)
  (define module-args (warn-command-args-module-args warn-command-args))
  (define config-args (warn-command-args-config-submod-args warn-command-args))
  (define modules (module-args->modules module-args))
  (match (length modules)
    [(== 0) (printf "No modules found\n")]
    [(== 1) (printf "Checking 1 module\n")]
    [num-modules (printf "Checking ~a modules\n" num-modules)])
  (flush-output)
  (if (warn-modules modules config-args) 1 0))

(module+ test
  (test-case "run-warn-command!"
    (define (test-command args)
      (define output (open-output-string))
      (define code
        (parameterize ([current-output-port output])
          (run-warn-command! args)))
      (list code (get-output-string output)))
    (test-case "no-warnings"
      (define no-warn-result
        (test-command
         (warn-command-args
          #:module (module-args 'collection
                                (list "syntax/warn/test-no-warnings")))))
      (define no-warn-expected-strs
        (list "Checking"
              "module"
              "raco warn: "
              "test-no-warnings/main.rkt"))
      (check-string-contains-all? (second no-warn-result)
                                  no-warn-expected-strs)
      (check-equal? (first no-warn-result) 0))
    (test-case "warnings"
      (define warn-result
        (test-command
         (warn-command-args
          #:module (module-args 'collection
                                (list "syntax/warn/test-warnings")))))
      (define warn-expected-strs
        (list "Checking"
              "module"
              "raco warn: "
              "test-warnings/main.rkt"
              "phase order"
              "suggested fix"))
      (check-string-contains-all? (second warn-result)
                                  warn-expected-strs)
      (check-equal? (first warn-result) 1))
    (test-case "warnings-suppressed"
      (define result
        (test-command
         (warn-command-args
          #:module (module-args 'collection
                                (list "syntax/warn/test-warnings-suppressed")))))
      (check-equal? (first result) 0))))

(module+ main
  (exit (run-warn-command! (parse-warn-command!))))
