(module version-case mzscheme 
  (require-for-syntax "version-misc.ss"
                      (lib "stx.ss" "syntax"))
  
  (provide version-case)
  
  
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
  
  
  (define-syntax (version-case stx)
    (define (eval-condition condition-stx)
      ;; TODO: should we do this more safely or in a more
      ;; controlled fashion?
      (parameterize ([current-namespace (make-namespace)])
        (with-handlers ([exn:fail?
                         (lambda (exn)
                           (raise-syntax-error #f
                                               (format "exn raised during compilation: ~a"
                                                       (exn-message exn))
                                               condition-stx))])
          (eval `(define version<= ,version<=))
          (eval `(define version>= ,version>=))
          (eval `(define version= ,version=))
          (eval `(define version< ,version<))
          (eval `(define version> ,version>))

          (eval condition-stx))))
    
    (syntax-case* stx (else) module-or-top-identifier=?
      [(_ [else code ...])
       (syntax/loc stx
         (begin code ...))]
      [(_ [condition code ...] other-clauses ...)
       (if (eval-condition (syntax condition))
           (syntax/loc stx
             (begin code ...))
           (syntax/loc stx
             (version-case other-clauses ...)))]
      [(_)
       (raise-syntax-error #f "no matching version condition" stx)])))