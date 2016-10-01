#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [fix-args
   (->* ()
        (#:flag-config (or/c config-args? #f)
         #:module (or/c module-args? #f)
         #:run-mode (or/c run-mode? #f)
         #:submod-config (or/c submod-args? #f))
        fix-args?)]
  [fix-args? predicate/c]
  [fix-args-module (-> fix-args? module-args?)]
  [fix-args-submod-config (-> fix-args? submod-args?)]
  [fix-args-run-mode (-> fix-args? run-mode?)]
  [fix-args-config
   (-> (or/c module-path? resolved-module-path? module-path-index?)
       fix-args?
       warning-config?)]
  [parse-fix-command! (-> fix-args?)]
  [parse-warn-command! (-> warn-args?)]
  [run-mode? flat-contract?]
  [warn-args
   (->* ()
        (#:flag-config (or/c config-args? #f)
         #:module (or/c module-args? #f)
         #:submod-config (or/c submod-args? #f))
        warn-args?)]
  [warn-args? predicate/c]
  [warn-args-flag-config (-> warn-args? config-args?)]
  [warn-args-module (-> warn-args? module-args?)]
  [warn-args-submod-config (-> warn-args? submod-args?)]
  [warn-args-config
   (-> (or/c module-path? resolved-module-path? module-path-index?)
       warn-args?
       warning-config?)]
  [write-module-count-message (-> exact-nonnegative-integer? void?)]))

(require racket/cmdline
         racket/function
         racket/match
         raco/command-name
         syntax/warn
         "config.rkt"
         "module.rkt")

(module+ test
  (require rackunit))


(define run-mode? (or/c 'wet 'dry))

(struct warn-args
  (flag-config module submod-config)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-warn-args)

(define (warn-args #:flag-config [flag-config #f]
                   #:module [module #f]
                   #:submod-config [submod-config #f])
  (make-warn-args (or flag-config (config-args))
                  (or module (module-args 'collection '()))
                  (or submod-config (submod-args))))

(struct fix-args
  (flag-config module run-mode submod-config)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-fix-args)

(define (fix-args #:flag-config [flag-config #f]
                  #:module [module #f]
                  #:run-mode [run-mode #f]
                  #:submod-config [submod-config #f])
  (make-fix-args (or flag-config (config-args))
                 (or module (module-args 'collection '()))
                 (or run-mode 'wet)
                 (or submod-config (submod-args))))

(module+ test
  (test-equal? "fix-args defaults"
               (fix-args)
               (fix-args #:flag-config (config-args)
                         #:module (module-args 'collection '())
                         #:run-mode 'wet
                         #:submod-config (submod-args))))

(define (warn-args-config mod args)
  (warning-config-merge (submod-args-config mod (warn-args-submod-config args))
                        (config-args->config (warn-args-flag-config args))))

(define (fix-args-config mod args)
  (warning-config-merge (submod-args-config mod (fix-args-submod-config args))
                        (config-args->config (fix-args-flag-config args))))

(define (check-kind k)
  (unless (member k (list 'file 'directory 'collection 'package))
    (raise-arguments-error
     (string->symbol (short-program+command-name))
     "expected an arg kind of file, directory, collection, or package"
     "--arg-kind" k)))

(define (check-run-mode m)
  (unless (run-mode? m)
    (raise-arguments-error
     (string->symbol (short-program+command-name))
     "expected a run mode of wet or dry"
     "--run-mode" m)))

(define (parse-warn-command!)
  (define kind-param (make-parameter 'collection))
  (define config-submod-param (make-parameter #f))
  (define config-submod-binding-param (make-parameter #f))
  (define suppressions-param (make-parameter '()))
  (define unsuppressions-param (make-parameter '()))
  (command-line
   #:program (short-program+command-name)
   #:once-any
   ["--arg-kind" kind
                 ("How to interpret the arguments as modules"
                  "One of file, directory, collection, or package"
                  "Defaults to file")
                 (define kind-sym (string->symbol kind))
                 (check-kind kind-sym)
                 (kind-param kind-sym)]
   [("-f" "--files") ("Interpret the arguments as files"
                      "Files are required as modules and checked"
                      "Equivalent to \"--arg-kind file\", default behavior")
                     (kind-param 'file)]
   [("-d" "--directories") ("Interpret the arguments as directories"
                            "Modules in directories are recursively checked"
                            "Equivalent to \"--arg-kind directory\"")
                           (kind-param 'directory)]
   [("-c" "--collections") ("Interpet the arguments as collections"
                            "Modules in collections are recursively checked"
                            "Equivalent to \"--arg-kind collection\"")
                           (kind-param 'collection)]
   [("-p" "--packages") ("Interpret the arguments as packages"
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
   #:multi
   ["--suppress" kind-str
                 ("Name of a warning kind to suppress")
                 (suppressions-param (cons (string->symbol kind-str)
                                           (suppressions-param)))]
   ["--unsuppress" kind-str
                   ("Name of a warning kind to unsuppress")
                   (unsuppressions-param (cons (string->symbol kind-str)
                                               (unsuppressions-param)))]
   #:args (arg . args)
   (warn-args
    #:flag-config (config-args #:suppress (reverse (suppressions-param))
                               #:unsuppress (reverse (unsuppressions-param)))
    #:module (module-args (kind-param) (cons arg args))
    #:submod-config (submod-args #:name (config-submod-param)
                                 #:binding (config-submod-binding-param)))))

(module+ test
  (test-case "parse-warn-command!"
    (define (parse/args . args)
      (parameterize ([current-command-line-arguments (list->vector args)])
        (parse-warn-command!)))
    (check-equal? (parse/args "-f" "foo" "bar")
                  (warn-args #:module (module-args 'file (list "foo" "bar"))))
    (check-equal? (parse/args "-d" "foo" "bar")
                  (warn-args #:module (module-args 'directory (list "foo" "bar"))))
    (check-equal? (parse/args "-c" "foo" "bar")
                  (warn-args #:module (module-args 'collection (list "foo" "bar"))))
    (check-equal? (parse/args "-p" "foo" "bar")
                  (warn-args #:module (module-args 'package (list "foo" "bar"))))
    (check-equal? (parse/args "--arg-kind" "file" "foo" "bar")
                  (warn-args #:module (module-args 'file (list "foo" "bar"))))
    (check-equal? (parse/args "--config-submod" "foo"
                              "--config-submod-binding" "bar"
                              "some/file")
                  (warn-args
                   #:module (module-args 'collection (list "some/file"))
                   #:submod-config (submod-args #:name 'foo #:binding 'bar)))
    (check-equal? (parse/args "--suppress" "k1"
                              "--suppress" "k2"
                              "--unsuppress" "k3"
                              "--unsuppress" "k4"
                              "some/file")
                  (warn-args
                   #:module (module-args 'collection (list "some/file"))
                   #:flag-config (config-args #:suppress '(k1 k2)
                                              #:unsuppress '(k3 k4))))))

(define (parse-fix-command!)
  (define kind-param (make-parameter 'collection))
  (define run-mode-param (make-parameter #f))
  (define config-submod-param (make-parameter #f))
  (define config-submod-binding-param (make-parameter #f))
  (define suppressions-param (make-parameter '()))
  (define unsuppressions-param (make-parameter '()))
  (command-line
   #:program (short-program+command-name)
   #:once-any
   ["--arg-kind" kind
                 ("How to interpret the arguments as modules"
                  "One of file, directory, collection, or package"
                  "Defaults to file")
                 (define kind-sym (string->symbol kind))
                 (check-kind kind-sym)
                 (kind-param kind-sym)]
   [("-f" "--files") ("Interpret the arguments as files"
                      "Files are required as modules and checked"
                      "Equivalent to \"--arg-kind file\", default behavior")
                     (kind-param 'file)]
   [("-d" "--directories") ("Interpret the arguments as directories"
                            "Modules in directories are recursively checked"
                            "Equivalent to \"--arg-kind directory\"")
                           (kind-param 'directory)]
   [("-c" "--collections") ("Interpet the arguments as collections"
                            "Modules in collections are recursively checked"
                            "Equivalent to \"--arg-kind collection\"")
                           (kind-param 'collection)]
   [("-p" "--packages") ("Interpret the arguments as packages"
                         "Modules in packages are recursively checked"
                         "Equivalent to \"--arg-kind package\"")
                        (kind-param 'package)]
   #:once-any
   ["--run-mode" mode
                 ("How to act on fixable warnings"
                  "One of \"dry\" or \"wet\""
                  "Defaults to \"wet\"")
                 (define mode-sym (string->symbol mode))
                 (check-run-mode mode-sym)
                 (run-mode-param mode-sym)]
   [("-D" "--dry") "Don't actually write any fixes to files"
                   (run-mode-param 'dry)]
   [("-E" "--wet") "Write fixes to files (default behavior)"
                   (run-mode-param 'wet)]
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
   #:multi
   ["--suppress" kind-str
                 ("Name of a warning kind to suppress")
                 (suppressions-param (cons (string->symbol kind-str)
                                           (suppressions-param)))]
   ["--unsuppress" kind-str
                   ("Name of a warning kind to unsuppress")
                   (unsuppressions-param (cons (string->symbol kind-str)
                                               (unsuppressions-param)))]
   #:args (arg . args)
   (fix-args
    #:flag-config (config-args #:suppress (reverse (suppressions-param))
                               #:unsuppress (reverse (unsuppressions-param)))
    #:module (module-args (kind-param) (cons arg args))
    #:run-mode (run-mode-param)
    #:submod-config (submod-args #:name (config-submod-param)
                                 #:binding (config-submod-binding-param)))))

(module+ test
  (define (parse/args . args)
    (parameterize ([current-command-line-arguments (list->vector args)])
      (parse-fix-command!)))
  (test-equal? "parse-fix-command! package args"
               (parse/args "-p" "foo" "bar" "baz")
               (fix-args #:module (module-args 'package
                                               (list "foo" "bar" "baz"))
                         #:run-mode 'wet))
  (test-equal? "parse-fix-command! dry mode"
               (parse/args "-Dp" "foo" "bar" "baz")
               (fix-args #:module (module-args 'package
                                               (list "foo" "bar" "baz"))
                         #:run-mode 'dry))
  (test-equal? "parse-fix-command! wet mode"
               (parse/args "-Ep" "foo" "bar" "baz")
               (fix-args #:module (module-args 'package
                                               (list "foo" "bar" "baz"))
                         #:run-mode 'wet))
  (test-equal? "parse-fix-command! mode arg"
               (parse/args "--run-mode" "dry" "foo" "bar")
               (fix-args #:module (module-args 'collection
                                               (list "foo" "bar"))
                         #:run-mode 'dry))
  (test-exn "parse-fix-command! bad run mode"
            exn:fail:contract?
            (thunk (parse/args "--run-mode" "nonsense" "foo" "bar")))
  (test-equal? "parse-fix-command! kind arg"
               (parse/args "--arg-kind" "collection" "foo" "bar")
               (fix-args #:module (module-args 'collection
                                               (list "foo" "bar"))
                         #:run-mode 'wet))
  (test-exn "parse-fix-command! bad kind"
            exn:fail:contract?
            (thunk (parse/args "--arg-kind" "nonsense" "foo" "bar")))
  (test-equal? "parse-fix-command! suppression flags"
               (parse/args "--suppress" "k1"
                           "--suppress" "k2"
                           "--unsuppress" "k3"
                           "--unsuppress" "k4"
                           "some/file")
               (fix-args #:module (module-args 'collection (list "some/file"))
                         #:flag-config (config-args #:suppress '(k1 k2)
                                                    #:unsuppress '(k3 k4)))))

(define (write-module-count-message mod-count)
  (match mod-count
    [(== 0) (write-string "No modules found\n")]
    [(== 1) (write-string "Checking 1 module\n")]
    [num-modules (write-string (format "Checking ~a modules\n" num-modules))])
  (flush-output))
