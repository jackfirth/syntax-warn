#lang racket/base

(require racket/format
         racket/list
         racket/string
         syntax/warn
         syntax/warn/private/string-lines
         "private/command.rkt"
         "private/config.rkt"
         "private/module.rkt"
         "private/read.rkt"
         "private/syntax-string.rkt")

(module+ test
  (require rackunit
           syntax/warn/private/rackunit-string))


(module+ test
  (define-warning-kind raco-test-kind))

(struct error-message-field (name value detailed?)
  #:constructor-name make-error-message-field
  #:omit-define-syntaxes
  #:transparent)

(define (error-message-field name value #:detailed? [detailed? #f])
  (make-error-message-field name value detailed?))

(define (error-message-field->string field)
  (define name (error-message-field-name field))
  (define value (error-message-field-value field))
  (define detailed? (error-message-field-detailed? field))
  (if detailed?
      (format "  ~a:\n   ~a" name (string-replace (~a value) "\n" "\n   "))
      (format "  ~a: ~a" name value)))
  
(define (error-message message
                       #:file [file #f]
                       #:line [line #f]
                       #:column [column #f]
                       #:err-source [err-source #f]
                       . fields)
  (define loc-pieces (list/filter file line column))
  (define loc (and (not (empty? loc-pieces))
                   (string-join (map ~a loc-pieces) ":")))
  (define message-line
    (string-join (list/filter loc err-source message) ": "))
  (define field-strs (map error-message-field->string fields))
  (string-join (cons message-line field-strs) "\n"))

(define (list/filter . vs) (filter values vs))

(define (format-warning warning)
  (define stx (syntax-warning-stx warning))
  (define fix (syntax-warning-fix warning))
  (define fields
    (list/filter (error-message-field 'source
                                      (syntax->string/line-numbers stx)
                                      #:detailed? #t)
                 (and fix
                      (error-message-field '|suggested fix|
                                           (syntax->string/line-numbers fix)
                                           #:detailed? #t))))
  (define kind-source
    (symbol->string (warning-kind-name (syntax-warning-kind warning))))
  (apply error-message
         (syntax-warning-message warning)
         #:file (syntax-source stx)
         #:line (syntax-line stx)
         #:column (syntax-column stx)
         #:err-source kind-source
         fields))

(module+ test
  (test-case "format-warning"
    (define test-stx
      (datum->syntax #'here
                     'foo
                     (list "/some/path/to/a/module.rkt"
                           8 ; line
                           3 ; column
                           123 ; position
                           3))) ; span
    (test-case "without a suggested fix"
      (define test-warning
        (syntax-warning #:message "not there"
                        #:kind raco-test-kind
                        #:stx test-stx))
      (check-equal? (format-warning test-warning)
                    "\
/some/path/to/a/module.rkt:8:3: raco-test-kind: not there
  source:
   8    foo"))
    (test-case "with a suggested fix"
      (define test-warning
        (syntax-warning #:message "not there"
                        #:kind raco-test-kind
                        #:stx test-stx
                        #:fix (datum->syntax test-stx 'bar test-stx)))
      (check-equal? (format-warning test-warning)
                    "\
/some/path/to/a/module.rkt:8:3: raco-test-kind: not there
  source:
   8    foo
  suggested fix:
   8    bar"))))

(define (print-warning warning)
  (printf (format-warning warning))
  (newline)
  (flush-output))

(define (warn-modules resolved-module-paths warn-args)
  (define any-warned? (box #f))
  (define warnings-namespace (make-base-namespace))
  (for ([modpath resolved-module-paths])
    (printf "raco warn: ~a\n" modpath)
    (flush-output)
    (define all-warnings
      (read-syntax-warnings/file modpath #:namespace warnings-namespace))
    (define config
      (warn-args-config modpath warn-args))
    (define warnings (filter-unsuppressed-warnings all-warnings config))
    (when (not (empty? warnings))
      (for-each print-warning warnings)
      (set-box! any-warned? #t)))
  (unbox any-warned?))

(define (run-warn-command! warn-args)
  (define modules (module-args->modules (warn-args-module warn-args)))
  (write-module-count-message (length modules))
  (if (warn-modules modules warn-args) 1 0))

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
         (warn-args
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
         (warn-args
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
         (warn-args
          #:module (module-args 'collection
                                (list "syntax/warn/test-warnings-suppressed")))))
      (check-equal? (first result) 0))))

(module+ main
  (exit (run-warn-command! (parse-warn-command!))))
