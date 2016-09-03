#lang racket/base

(require compiler/module-suffix
         pkg/lib
         racket/contract/base
         racket/list
         racket/path
         syntax/modresolve)

(define arg-resolver/c
  (->* (string?) (#:source-name symbol?)
       (listof module-path?)))

(provide
 (contract-out
  [directory-warn-modules arg-resolver/c]
  [collection-warn-modules arg-resolver/c]
  [package-warn-modules arg-resolver/c]))

(require (only-in (submod compiler/commands/test paths)
                  collection-paths))


(define no-such-directory-exn-msg-format
  "~a: no such directory;\n directory: ~a\n full: ~a")

(define no-such-collection-exn-msg-format
  "~a: no such collection;\n collection: ~a")

(define no-such-package-exn-msg-format
  "~a: no such package;\n package: ~a")

(define (directory-warn-modules dir #:source-name [source-name #f])
  (define full-path
    (simplify-path (expand-user-path (path->directory-path dir))))
  (unless (directory-exists? full-path)
    (define msg
      (format no-such-directory-exn-msg-format
              (or source-name 'error)
              dir
              full-path))
    (raise-user-error msg))
  (directory-warn-modules/path full-path))

(define (collection-warn-modules collect #:source-name [source-name #f])
  (define collect-paths (collection-paths collect))
  (when (empty? collect-paths)
    (define msg
      (format no-such-collection-exn-msg-format
              (or source-name 'error)
              collect))
    (raise-user-error msg))
  (append-map directory-warn-modules/path collect-paths))

(define current-package-info-cache (make-parameter (make-hash)))

(define (package-warn-modules pkg #:source-name [source-name #f])
  (define maybe-pkg-dir
    (pkg-directory pkg #:cache (current-package-info-cache)))
  (unless maybe-pkg-dir
    (define msg
      (format no-such-package-exn-msg-format
              (or source-name 'error)
              pkg))
    (raise-user-error msg))
  (directory-warn-modules/path maybe-pkg-dir))

(define (directory-warn-modules/path dir-path)
  (define-values (files subdirs)
    (partition file-exists? (directory-list dir-path #:build? #t)))
  (define module-files (filter module-file? files))
  (define all-files
    (append module-files
            (append-map directory-warn-modules/path subdirs)))
  (map resolve-module-path all-files))

(define (module-file? file-path)
  (define maybe-ext (path-get-extension file-path))
  (and maybe-ext
       (member (subbytes maybe-ext 1)
               (get-module-suffixes))))

