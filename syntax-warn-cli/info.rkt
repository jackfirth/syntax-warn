#lang info
(define name "syntax-warn-cli")
(define collection "syntax")
(define raco-commands
  '(("warn" (submod syntax/warn/raco-warn main) "Check for syntax warnings" 20)
    ("fix" (submod syntax/warn/raco-fix main) "Fix syntax warnings" 20)))
(define deps '("rackunit-lib"
               "syntax-warn-lang"
               "base"
               "compiler-lib"
               "syntax-warn-base"))
(define build-deps '("rackunit-lib"))
