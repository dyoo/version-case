(module version-case mzscheme
    
  #;(require-for-syntax "version-misc.ss")
  
  (provide version-case)

  #'(provide (for-syntax (all-from "version-misc.ss")))
  
  
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
  
  
  (define-for-syntax (loop stx clauses)
    (syntax-case clauses ()
      [()
       (raise-syntax-error #f "no matching version condition" stx)]
      [([test code ...] ...)
       (with-syntax ([name (syntax/loc clauses the-macro)]
                     [transformer
                      (syntax/loc stx
                        (lambda (stx*)
                          (cond [test
                                 (syntax-local-introduce
                                  (syntax/loc stx* 
                                    (begin code ...)))]
                                ...)))])
         (case (syntax-local-context)
           [(expression)
            (syntax/loc stx
              (let-syntax ([name transformer])
                (name)))]
           [else
            (syntax/loc stx
              (begin
                (define-syntax name transformer)
                (name)))]))]))
  
  
  
  (define-syntax (version-case stx)
    (syntax-case stx ()
      [(_ clauses ...)
       (loop stx #'(clauses ...))])))