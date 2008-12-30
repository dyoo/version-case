(module version-case mzscheme   
  (provide version-case)
  (require-for-syntax "version-misc.ss")
  
  
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
  
  
  (define-for-syntax usage-message "Usage: (version-case [test code] ... [else ...]))")
  
  ;; The new implementation is thanks to Carl Eastlund.
  (define-for-syntax (new-implementation stx)
    (with-syntax ([version-case (datum->syntax-object stx 'version-case)])
      (syntax/loc stx
        (begin
          (provide-for-syntax (all-from "version-misc.ss"))
          (require-for-syntax (prefix 399: scheme/base))
          (provide-for-syntax (rename 399:else else))
          (define-syntax (version-case stx)
            (syntax-case stx ()
              [(_ [test code (... ...)] (... ...))
               (not (null? (syntax->list (syntax ((test code (... ...)) (... ...))))))
               (with-syntax ([name (syntax/loc stx the-macro)]
                             [transformer
                              (syntax/loc stx
                                (lambda (stx*)
                                  (399:cond [test
                                             (syntax-local-introduce
                                              (syntax/loc stx* (begin code (... ...))))]
                                            (... ...))))])
                 (case (syntax-local-context)
                   [(expression)
                    (syntax/loc stx
                      (let-syntax ([name transformer])
                        (name)))]
                   [else
                    (syntax/loc stx
                      (begin
                        (define-syntax name transformer)
                        (name)))]))]
              [else
               (raise-syntax-error 
                #f 
                usage-message
                stx)]))))))
  
  
  ;; The old version uses eval because I haven't figured out a good way to expose the
  ;; version-misc identifiers at the syntax phase without provide-for-syntax.
  (define-for-syntax (old-implementation stx)
    (with-syntax ([version-case (datum->syntax-object stx 'version-case)])
      (syntax/loc stx
        (begin
          (require-for-syntax "version-misc.ss")
          (require-for-syntax (lib "stx.ss" "syntax"))
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
              [(_ [else code (... ...)])
               (syntax/loc stx
                 (begin code (... ...)))]
              [(_ [condition code (... ...)] other-clauses (... ...))
               (if (eval-condition (syntax condition))
                   (syntax/loc stx
                     (begin code (... ...)))
                   (syntax/loc stx
                     (version-case other-clauses (... ...))))]
              [(_)
               (raise-syntax-error #f usage-message stx)]))))))
    
  
  
  ;; We choose at compile time of version-case which implementation can be used.
  (define-syntax (choose-version-case-implementation stx)
    (cond
      [(version< (version) "399")
       (old-implementation stx)]
      [else
       (new-implementation stx)]))
  

  (choose-version-case-implementation))
