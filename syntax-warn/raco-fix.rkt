#lang racket/base

(require racket/cmdline
         racket/match
         raco/command-name
         "private/module.rkt"
         "main.rkt")

(struct fix-args (module-args run-mode) #:transparent)

(define (parse-warn-command!)
  (define kind-param (make-parameter 'file))
  (define run-mode-param (make-parameter 'wet))
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
   #:once-each
   [("-D" "--dry-run") "Don't actually write any fixes to files"
                       (run-mode-param 'dry)]
   #:args (arg . args)
   (fix-args (module-args (kind-param) (cons arg args))
             (run-mode-param))))

(define (check-kind! k)
  (unless (member k (list 'file 'directory 'collection 'package))
    (raise-arguments-error
     (string->symbol (short-program+command-name))
     "expected an arg kind of file, directory, collection, or package"
     "--arg-kind" k)))

(define (write-module-count-message mod-count)
  (match mod-count
    [(== 0) (write-string "No modules found\n")]
    [(== 1) (write-string "Checking 1 module\n")]
    [num-modules (write-string (format "Checking ~a modules\n" num-modules))])
  (flush-output))

(define (write-warning-fix-message #:module-path mod
                                   #:num-warnings num-warnings
                                   #:num-fixes num-fixes
                                   #:run-mode mode)
  (unless (zero? num-fixes)
    (define one-warning? (= num-warnings 1))
    (define dry-run? (equal? mode 'dry))
    (define message
      (format "~a: ~a warning~a, ~a ~a\n"
              mod
              num-warnings
              (if one-warning? "" "s")
              (if dry-run? "would fix" "fixing")
              (if one-warning? "" num-fixes)))
    (write-string message)))

(define (fix-warnings! args)
  (match-define (fix-args mod-args mode) args)
  (define mods (module-args->modules mod-args))
  (write-module-count-message (length mods))
  (for ([mod mods])
    (define warnings (read-module-warnings mod))
    (define warnings/fixes (filter syntax-warning/fix? warnings))
    (write-warning-fix-message #:module-path mod
                               #:num-warnings (length warnings)
                               #:num-fixes (length warnings/fixes)
                               #:run-mode mode)))

(module+ main
  (fix-warnings! (parse-warn-command!)))
