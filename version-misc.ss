(module version-misc mzscheme
  (provide version<=)
  
  (define-struct mz-version (major minor))
  
  (define (string->version str)
    (let ((ver (regexp-match #rx"^([0-9]+)(\\.([0-9]+))?$" str)))
      (if ver
          (make-mz-version
           (string->number (list-ref ver 1))
           (if (list-ref ver 3)
               (string->number (list-ref ver 3))
               0))
          #f)))
  
  (define (version<= a b)
    (let ([a (string->version a)]
          [b (string->version b)])
      (or (<= (mz-version-major a) (mz-version-major b))
          (and (= (mz-version-major a) (mz-version-major b))
               (<= (mz-version-minor a) (mz-version-minor b)))))))