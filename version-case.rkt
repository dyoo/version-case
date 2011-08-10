#lang racket/base
(require (for-syntax "version-misc.rkt"
                     racket/base))

;; version-case: SYNTAX
;; Conditionally include code based on current version number.
;; Usage:
;;
;; (version-case [clause code] ... 
;;               [else ...])
;;
;; where clause has access to the mzscheme primitives.  In addition,
;; the following functions are available:
;;
;;     version<=: string string -> boolean
;;     version>=: string string -> boolean
;;     version=: string string -> boolean
;;     version<: string string -> boolean
;;     version>: string string -> boolean
;;
;; to make it easier to build the conditional clauses.


;; We choose at compile time of version-case which implementation can be used.
(define-syntax (choose-version-case-implementation stx)
  (cond
   [(version< (version) "399")
    (syntax/loc stx 
      (begin
        (require "private/old-version-case.rkt")
        (provide (all-from-out "private/old-version-case.rkt"))))]
   [else
    (syntax/loc stx
      (begin
        (require "private/new-version-case.rkt")
        (provide (all-from-out "private/new-version-case.rkt"))))]))


(choose-version-case-implementation)
