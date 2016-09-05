#lang racket/base

(require racket/system)

(module+ test
  (require rackunit
           warn/private/rackunit-string))


(struct system-result (code stdout stderr) #:transparent)

(define (system/result system-str)
  (define stdout-str-port (open-output-string))
  (define stderr-str-port (open-output-string))
  (define exit-code
    (parameterize ([current-output-port stdout-str-port]
                   [current-error-port stderr-str-port])
      (system/exit-code system-str)))
  (define stdout (get-output-string stdout-str-port))
  (define stderr (get-output-string stderr-str-port))
  (system-result exit-code stdout stderr))

(define (run-warn-command collect)
  (system/result (format "raco warn -c ~a" collect)))

(module+ test
  (test-case "Checking a module with no warnings"
    (define result (run-warn-command "warn/test-no-warnings"))
    (check-equal? (system-result-code result) 0)
    (check-string-contains? (system-result-stdout result)
                            "Checking 1 module\n")
    (check-equal? (system-result-stderr result) ""))
  (test-case "Checking a module with warnings"
    (define result (run-warn-command "warn/test-warnings"))
    (check-equal? (system-result-code result) 1)
    (check-string-contains-all? (system-result-stdout result)
                                (list "Checking 1 module\n"
                                      "syntax-warn-test/test-warnings/main.rkt"
                                      "phase order"
                                      "suggested fix"
                                      "require"
                                      "for-syntax"))
    (check-equal? (system-result-stderr result) "")))
