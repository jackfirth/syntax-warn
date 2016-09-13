#lang racket/base

(require racket/cmdline
         racket/file
         racket/match
         raco/command-name
         syntax/warn
         syntax/warn/private/warn-config
         syntax/warn/private/syntax-srcloc
         syntax/warn/private/syntax-string
         "private/config.rkt"
         "private/module.rkt"
         "private/string-delta.rkt")

(module+ test
  (require racket/function
           racket/port
           rackunit
           syntax/warn/private/rackunit-string))


(struct fix-args
  (module-args run-mode config-submod)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-fix-args)

(define (fix-args #:module [mod #f]
                  #:run-mode [run-mode 'wet]
                  #:config-submod [config-submod #f])
  (make-fix-args (or mod (module-args 'file '()))
                 run-mode
                 (or config-submod (config-submod-args))))

(define (parse-warn-command!)
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
  (test-case "parse-warn-command! package args"
    (define args
      (vector "-p" "foo" "bar" "baz"))
    (parameterize ([current-command-line-arguments args])
      (check-equal? (parse-warn-command!)
                    (fix-args #:module (module-args 'package
                                                    (list "foo" "bar" "baz"))
                              #:run-mode 'wet))))
  (test-case "parse-warn-command! dry mode"
    (define args
      (vector "-Dp" "foo" "bar" "baz"))
    (parameterize ([current-command-line-arguments args])
      (check-equal? (parse-warn-command!)
                    (fix-args #:module (module-args 'package
                                                    (list "foo" "bar" "baz"))
                              #:run-mode 'dry))))
  (test-case "parse-warn-command! kind arg"
    (define args
      (vector "--arg-kind" "collection" "foo" "bar"))
    (parameterize ([current-command-line-arguments args])
      (check-equal? (parse-warn-command!)
                    (fix-args #:module (module-args 'collection
                                                    (list "foo" "bar"))
                              #:run-mode 'wet))))
  (test-case "parse-warn-command! bad kind"
    (define args
      (vector "--arg-kind" "nonsense" "foo" "bar"))
    (parameterize ([current-command-line-arguments args])
      (check-exn exn:fail:contract? parse-warn-command!))))

(define (write-module-count-message mod-count)
  (match mod-count
    [(== 0) (write-string "No modules found\n")]
    [(== 1) (write-string "Checking 1 module\n")]
    [num-modules (write-string (format "Checking ~a modules\n" num-modules))])
  (flush-output))

(define (write-warning-fix-message #:module-path mod
                                   #:num-warnings num-warnings
                                   #:num-fixes num-fixes
                                   #:num-deltas num-deltas
                                   #:run-mode mode)
  (cond [(zero? num-fixes)
         (write-string (format "raco fix: ~a\n" mod))]
        [else
         (define one-warning? (= num-warnings 1))
         (define dry-run? (equal? mode 'dry))
         (define message
           (format "raco fix: ~a: ~a warning~a, ~a ~a~a\n"
                   mod
                   num-warnings
                   (if one-warning? "" "s")
                   (if dry-run? "would fix" "fixing")
                   (if one-warning? "" num-fixes)
                   (if (equal? num-deltas num-fixes)
                       ""
                       (format ", ~a conflicting fixes discarded"
                               (- num-fixes num-deltas)))))
         (write-string message)])
  (flush-output))

(define (syntax-warning/fix->string-delta warning/fix)
  (define loc (syntax-complete-srcloc (syntax-warning-stx warning/fix)))
  (string-delta (sub1 (complete-srcloc-position loc))
                (complete-srcloc-position-span loc)
                (syntax->string (syntax-warning-fix warning/fix))))

(define (fix-warnings! args)
  (define mod-args (fix-args-module-args args))
  (define mode (fix-args-run-mode args))
  (define submod-args (fix-args-config-submod args))
  (define mods (module-args->modules mod-args))
  (write-module-count-message (length mods))
  (define warnings-namespace (make-base-namespace))
  (for ([mod mods])
    (define all-warnings (read-syntax-warnings/file mod #:namespace warnings-namespace))
    (define config (require-warning-config-submod mod submod-args))
    (define warnings (filter-unsuppressed-warnings all-warnings config))
    (define warnings/fixes (filter syntax-warning/fix? warnings))
    (define deltas
      (prune-string-deltas (map syntax-warning/fix->string-delta warnings/fixes)))
    (write-warning-fix-message #:module-path mod
                               #:num-warnings (length warnings)
                               #:num-fixes (length warnings/fixes)
                               #:num-deltas (length deltas)
                               #:run-mode mode)
    (unless (equal? mode 'dry)
      (define mod-str (file->string mod))
      (define mod-str/deltas-applied (apply-pruned-string-deltas mod-str deltas))
      (define (write-deltas output-port _)
        (write-string mod-str/deltas-applied output-port))
      (call-with-atomic-output-file mod write-deltas)
      (void))))

(module+ test
  (test-case "fix-warnings!"
    (define test-args
      (fix-args #:module (module-args 'package (list "syntax-warn-cli"))
                #:run-mode 'dry))
    (define (fix-test-warnings!)
      (with-output-to-string (thunk (fix-warnings! test-args))))
    (check-not-exn fix-test-warnings!)
    (define expected-output-strs
      (list "Checking"
            "modules"
            "syntax-warn-cli"
            "raco fix:"
            "test-no-warnings/main.rkt"
            "test-warnings/main.rkt"
            "would fix"
            "1 warning"))
    (check-string-contains-all? (fix-test-warnings!)
                                expected-output-strs)))

(module+ main (fix-warnings! (parse-warn-command!)))
