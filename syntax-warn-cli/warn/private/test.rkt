#lang racket/base

(require racket/system)

(module+ test
  (require rackunit
           syntax/warn/private/rackunit-string))


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

(module+ test
  (test-case "Checking a module with no warnings"
    (define result (system/result "raco warn -c syntax/warn/test-no-warnings"))
    (check-equal? (system-result-stderr result) "")
    (check-equal? (system-result-code result) 0)
    (check-string-contains? (system-result-stdout result)
                            "Checking 1 module\n"))
  (test-case "Checking a module with warnings"
    (define result (system/result "raco warn -c syntax/warn/test-warnings"))
    (check-equal? (system-result-stderr result) "")
    (check-equal? (system-result-code result) 1)
    (check-string-contains-all? (system-result-stdout result)
                                (list "Checking 1 module\n"
                                      "test-warnings/main.rkt"
                                      "phase order"
                                      "suggested fix"
                                      "require"
                                      "for-syntax")))
  (test-case "Fixing a module with no warnings"
    (define result (system/result "raco fix -c syntax/warn/test-no-warnings"))
    (check-equal? (system-result-stderr result) "")
    (check-equal? (system-result-code result) 0)
    (define expected-stdout-strings
      (list "Checking 1 module\n"
            "test-no-warnings/main.rkt"))
    (check-string-contains-all? (system-result-stdout result)
                                expected-stdout-strings))
  (test-case "Fixing a module with warnings in dry run"
    (define result (system/result "raco fix -cD syntax/warn/test-warnings"))
    (check-equal? (system-result-stderr result) "")
    (check-equal? (system-result-code result) 0)
    (define expected-stdout-strings
      (list "Checking 1 module\n"
            "test-warnings/main.rkt"
            "would fix"))
    (check-string-contains-all? (system-result-stdout result)
                                expected-stdout-strings)))
