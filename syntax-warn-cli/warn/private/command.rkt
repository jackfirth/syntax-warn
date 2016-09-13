#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [fix-args
   (->* ()
        (#:module (or/c module-args? #f)
         #:run-mode (or/c run-mode? #f)
         #:submod-config (or/c submod-config-args? #f))
        fix-args?)]
  [fix-args? predicate/c]
  [fix-args-module (-> fix-args? module-args?)]
  [fix-args-submod-config (-> fix-args? submod-config-args?)]
  [fix-args-run-mode (-> fix-args? run-mode?)]
  [parse-fix-command! (-> fix-args?)]
  [parse-warn-command! (-> warn-args?)]
  [warn-args
   (->* ()
        (#:module (or/c module-args? #f)
         #:submod-config (or/c submod-config-args? #f))
        warn-args?)]
  [warn-args? predicate/c]
  [warn-args-module (-> warn-args? module-args?)]
  [warn-args-submod-config (-> warn-args? submod-config-args?)]
  [write-module-count-message (-> exact-nonnegative-integer? void?)]))

(require racket/cmdline
         racket/match
         raco/command-name
         "config.rkt"
         "module.rkt")

(module+ test
  (require rackunit))


(define run-mode? (or/c 'wet 'dry))

(struct warn-args
  (module submod-config)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-warn-args)

(define (warn-args #:module [module #f]
                   #:submod-config [submod-config #f])
  (make-warn-args (or module (module-args 'file '()))
                  (or submod-config (submod-config-args))))

(struct fix-args
  (module submod-config run-mode)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-fix-args)

(define (fix-args #:module [mod #f]
                  #:submod-config [submod-config #f]
                  #:run-mode [run-mode 'wet])
  (make-fix-args (or mod (module-args 'file '()))
                 (or submod-config (submod-config-args))
                 run-mode))

(define (check-kind! k)
  (unless (member k (list 'file 'directory 'collection 'package))
    (raise-arguments-error
     (string->symbol (short-program+command-name))
     "expected an arg kind of file, directory, collection, or package"
     "--arg-kind" k)))

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
   (warn-args
    #:module (module-args (kind-param) (cons arg args))
    #:submod-config (submod-config-args
                     #:submod-name (config-submod-param)
                     #:binding-name (config-submod-binding-param)))))

(module+ test
  (test-case "parse-warn-command!"
    (define (parse/args args)
      (parameterize ([current-command-line-arguments args])
        (parse-warn-command!)))
    (check-equal? (parse/args (vector "-f" "foo" "bar"))
                  (warn-args #:module (module-args 'file (list "foo" "bar"))))
    (check-equal? (parse/args (vector "-d" "foo" "bar"))
                  (warn-args #:module (module-args 'directory (list "foo" "bar"))))
    (check-equal? (parse/args (vector "-c" "foo" "bar"))
                  (warn-args #:module (module-args 'collection (list "foo" "bar"))))
    (check-equal? (parse/args (vector "-p" "foo" "bar"))
                  (warn-args #:module (module-args 'package (list "foo" "bar"))))
    (check-equal? (parse/args (vector "--arg-kind" "file" "foo" "bar"))
                  (warn-args #:module (module-args 'file (list "foo" "bar"))))
    (check-equal? (parse/args (vector "--config-submod" "foo"
                                      "--config-submod-binding" "bar"
                                      "some/file"))
                  (warn-args
                   #:module (module-args 'file (list "some/file"))
                   #:submod-config (submod-config-args #:submod-name 'foo
                                                       #:binding-name 'bar)))))

(define (parse-fix-command!)
  (define kind-param (make-parameter 'file))
  (define run-mode-param (make-parameter 'wet))
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
   [("-D" "--dry-run") "Don't actually write any fixes to files"
                       (run-mode-param 'dry)]
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
   (fix-args
    #:module (module-args (kind-param) (cons arg args))
    #:run-mode (run-mode-param)
    #:submod-config (submod-config-args
                     #:submod-name (config-submod-param)
                     #:binding-name (config-submod-binding-param)))))

(module+ test
  (test-case "parse-fix-command! package args"
    (define args
      (vector "-p" "foo" "bar" "baz"))
    (parameterize ([current-command-line-arguments args])
      (check-equal? (parse-fix-command!)
                    (fix-args #:module (module-args 'package
                                                    (list "foo" "bar" "baz"))
                              #:run-mode 'wet))))
  (test-case "parse-fix-command! dry mode"
    (define args
      (vector "-Dp" "foo" "bar" "baz"))
    (parameterize ([current-command-line-arguments args])
      (check-equal? (parse-fix-command!)
                    (fix-args #:module (module-args 'package
                                                    (list "foo" "bar" "baz"))
                              #:run-mode 'dry))))
  (test-case "parse-fix-command! kind arg"
    (define args
      (vector "--arg-kind" "collection" "foo" "bar"))
    (parameterize ([current-command-line-arguments args])
      (check-equal? (parse-fix-command!)
                    (fix-args #:module (module-args 'collection
                                                    (list "foo" "bar"))
                              #:run-mode 'wet))))
  (test-case "parse-fix-command! bad kind"
    (define args
      (vector "--arg-kind" "nonsense" "foo" "bar"))
    (parameterize ([current-command-line-arguments args])
      (check-exn exn:fail:contract? parse-fix-command!))))

(define (write-module-count-message mod-count)
  (match mod-count
    [(== 0) (write-string "No modules found\n")]
    [(== 1) (write-string "Checking 1 module\n")]
    [num-modules (write-string (format "Checking ~a modules\n" num-modules))])
  (flush-output))
