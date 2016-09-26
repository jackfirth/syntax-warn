#lang racket/base

(require racket/cmdline
         racket/file
         racket/match
         raco/command-name
         syntax/warn
         syntax/warn/private/syntax-srcloc
         "private/syntax-string.rkt"
         "private/command.rkt"
         "private/config.rkt"
         "private/module.rkt"
         "private/string-delta.rkt")

(module+ test
  (require racket/function
           racket/port
           rackunit
           syntax/warn/private/rackunit-string))

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
  (define mod-args (fix-args-module args))
  (define mode (fix-args-run-mode args))
  (define mods (module-args->modules mod-args))
  (write-module-count-message (length mods))
  (define warnings-namespace (make-base-namespace))
  (for ([mod mods])
    (define all-warnings (read-syntax-warnings/file mod #:namespace warnings-namespace))
    (define config (fix-args-config mod args))
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

(module+ main (fix-warnings! (parse-fix-command!)))
