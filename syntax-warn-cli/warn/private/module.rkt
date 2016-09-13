#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [kind/c flat-contract?]
  [struct module-args
    ([kind kind/c]
     [strs (listof string?)])]
  [module-args->modules (-> module-args? (listof path?))]))

(require (only-in (submod compiler/commands/test paths)
                  collection-paths)
         compiler/module-suffix
         pkg/lib
         racket/list
         syntax/modresolve)


(define kind/c (or/c 'file 'directory 'collection 'package))
(struct module-args (kind strs) #:transparent)

(define (module-args->modules args)
  (define strs (module-args-strs args))
  (case (module-args-kind args)
    [(file) (map string->path strs)]
    [(directory) (append-map directory-warn-modules strs)]
    [(collection) (append-map collection-warn-modules strs)]
    [(package) (append-map package-warn-modules strs)]))

(define no-such-directory-exn-msg-format
  "~a: no such directory;\n directory: ~a\n full: ~a")

(define no-such-collection-exn-msg-format
  "~a: no such collection;\n collection: ~a")

(define no-such-package-exn-msg-format
  "~a: no such package;\n package: ~a")

(define (clean-directory-path path)
  (simplify-path (expand-user-path (path->directory-path path))))

(define (directory-warn-modules dir #:source-name [source-name #f])
  (define full-path (clean-directory-path dir))
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
  (define cleaned-path (clean-directory-path dir-path))
  (define-values (files subdirs)
    (partition file-exists? (directory-list cleaned-path #:build? #t)))
  (define module-files (filter module-file? files))
  (append* (map resolve-module-path/backported module-files)
           (map directory-warn-modules/path subdirs)))

(define (module-file? file-path)
  (define maybe-ext (path-get-extension/backported file-path))
  (and maybe-ext
       (member (subbytes maybe-ext 1)
               (get-module-suffixes))))

;; These utilities are backported from Racket 6.6 to support
;; earlier versions

(define (resolve-module-path/backported path)
  ;; Pre-6.6 implementations behave the same but require
  ;; the second arg
  (resolve-module-path path #f))

(define (file-name/backported who name dir-ok?)
  (unless (or (path-string? name)
              (path-for-some-system? name))
    (raise-argument-error who "(or/c path-string? path-for-some-system?)" name))
  (let-values ([(base file dir?) (split-path name)])
    (and (or dir-ok? (not dir?))
         (path-for-some-system? file) file)))

(define (path-get-extension/backported name)
  (let* ([name (file-name/backported 'path-get-extension name #t)]
         [name (and name (path->bytes name))])
    (cond [(and name (regexp-match #rx#"(?<=.)([.][^.]+)$" name)) => cadr]
          [else #f])))
