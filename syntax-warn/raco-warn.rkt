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
  (require rackunit
           "private/rackunit-port.rkt"
           "private/rackunit-string.rkt"))


(define (srcloc-location-string srcloc)
  (format "~a:~a"
          (srcloc-source srcloc)
          (srcloc-line srcloc)))

(define (separator-format sep width)
  (define separator (make-string width sep))
  (string-append-lines separator "~a" separator ""))

(define (format-warning warning #:separator-char [sep #\-] #:separator-width [width 80])
  (define warning-message
    (format "~a ~a"
            (srcloc-location-string (syntax-warning-location warning))
            (syntax-warning-message warning)))
  (define fix (syntax-warning-fix warning))
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
               (syntax->string/line-numbers (suggested-fix-original-stx fix)
                                            #:indent-spaces 3)
               ""
               "suggested fix:"
               ""
               (syntax->string/line-numbers (suggested-fix-replacement-stx fix)
                                            #:indent-spaces 3))
              warning-message)))

(module+ test
  (test-case "Formatted warning without a suggested fix"
    (define formatted-warning
      (format-warning (syntax-warning (syntax-srcloc #'here) "not there" #f)))
    (check-string-contains? formatted-warning "raco-warn.rkt")
    (check-string-contains? formatted-warning "not there")
    (check-string-has-trailing-newline? formatted-warning))
  (test-case "Formatted warning with a suggested fix"
    (define warning/fix
      (syntax-warning (syntax-srcloc #'foo)
                      "use a different name"
                      (suggested-fix #'foo #'bar)))
    (define expected-message-strings
      (list "----------------"
            "raco-warn.rkt"
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
                 (check-kind! kind)
                 (kind-param kind)]
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
   #:args (module-arg . module-args)
   (warn-command-args (kind-param)
                      (cons module-arg module-args))))

(define (check-kind! k)
  (unless (member k (list 'file 'directory 'collection 'package))
    (raise-arguments-error
     (string->symbol (short-program+command-name))
     "expected an arg kind of file, directory, collection, or package"
     "--arg-kind" k)))

(struct warn-command-args
  (kind module-args)
  #:transparent)

(define (warn-command-args->modules-to-check args)
  (define kind (warn-command-args-kind args))
  (define module-args (warn-command-args-module-args args))
  (case kind
    [(file) module-args]
    [(directory) (append-map directory-warn-modules module-args)]
    [(collection) (append-map collection-warn-modules module-args)]
    [(package) (append-map package-warn-modules module-args)]))

(define (warn-modules resolved-module-paths)
  (define any-warned? (box #f))
  (for ([modpath resolved-module-paths])
    (for ([warning (read-modpath-warnings modpath)])
      (set-box! any-warned? #t)
      (print-warning warning)))
  (unbox any-warned?))

(define (read-modpath-warnings modpath)
  (define (read-modpath-expansion)
    (with-input-from-file modpath #:mode 'text
      (thunk
       (port-count-lines! (current-input-port))
       (parameterize ([current-namespace (make-base-namespace)])
         (expand-syntax
          (namespace-syntax-introduce
           (read-syntax modpath)))))))
  (syntax-warnings
   (with-module-reading-parameterization read-modpath-expansion)))
  
(module+ main
  (define modules (warn-command-args->modules-to-check (parse-warn-command!)))
  (match (length modules)
    [(== 0) (printf "No modules found\n")]
    [(== 1) (printf "Checking 1 module\n")]
    [num-modules (printf "Checking ~a modules\n" num-modules)])
  (flush-output)
  (when (warn-modules modules)
    (exit 1)))
