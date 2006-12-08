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
  ;; the following two functions are available:
  ;;
  ;;     string->version: string -> version
  ;;
  ;;     version<=: version version -> boolean
  ;;
  ;; to make it easier to build the conditional clauses.
  
  
  
  ;; The defininitions of mz-version, string->mz-version, and version<= were
  ;; adapted (copied and pasted) from PLaneT's implementation
  ;; (planet/private/planet-shared.ss).
  ;; string->version : string -> mz-version | #f
  ;; version<= : string string -> boolean
  ;; determines if a is the version string of an earlier mzscheme release than b ;; [n.b. this relies on a guarantee from Matthew that mzscheme version
  ;; x1.y1 is older than version x2.y2 iff x1<x2 or x1=x2 and y1<y2]
  
  
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