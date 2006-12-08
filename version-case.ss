(module version-case mzscheme 
  (require-for-syntax (lib "stx.ss" "syntax"))
  
  (provide version-case)
  
  ;; version-case: SYNTAX
  ;; Conditionally include code based on current version number.
  ;; Usage:
  ;;
  ;; (version-case [clause code] ... 
  ;;               [else ...])
  ;;
  ;; where clause has access to the mzscheme primitives, in addition to:
  ;;
  ;; string->mz-version: string -> version
  ;;
  ;; version<=: version version -> boolean
  ;;
  ;; to compare.
  (define-syntax (version-case stx)
    (define (eval-condition condition-stx)
      ;; todo: should we do this more safely or in a more
      ;; controlled fashion?
      (parameterize ([current-namespace (make-namespace)])
        
        
        ;; The defininitions of mz-version, string->mz-version, and version<= were
        ;; adapted (copied and pasted) from PLaneT's implementation
        ;; (planet/private/planet-shared.ss).
        
        (eval '(define-struct mz-version (major minor)))
        
        ;; string->mz-version : string -> mz-version | #f
        (eval '(define (string->version str)
                 (let ((ver (regexp-match #rx"^([0-9]+)(\\.([0-9]+))?$" str)))
                   (if ver
                       (make-mz-version
                        (string->number (list-ref ver 1))
                        (if (list-ref ver 3)
                            (string->number (list-ref ver 3))
                            0))
                       #f))))
        
        ;; version<= : mz-version mz-version -> boolean
        ;; determines if a is the version string of an earlier mzscheme release than b ;; [n.b. this relies on a guarantee from Matthew that mzscheme version
        ;; x1.y1 is older than version x2.y2 iff x1<x2 or x1=x2 and y1<y2]
        (eval '(define (version<= a b)
                 (or (<= (mz-version-major a) (mz-version-major b))
                     (and (= (mz-version-major a) (mz-version-major b))
                          (<= (mz-version-minor a) (mz-version-minor b))))))
        
        (eval condition-stx)))
    
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