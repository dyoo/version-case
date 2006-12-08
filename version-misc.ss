(module version-misc mzscheme
  (provide version<=)
  
  
  ;; The definitions of mz-version, string->version, and version<= were
  ;; adapted (copied and pasted) from PLaneT's implementation in
  ;; (planet/private/planet-shared.ss).
  
  (define-struct mz-version (major minor))
  
  ;; string->version : string -> mz-version | #f
  (define (string->version str)
    (let ((ver (regexp-match #rx"^([0-9]+)(\\.([0-9]+))?$" str)))
      (if ver
          (make-mz-version
           (string->number (list-ref ver 1))
           (if (list-ref ver 3)
               (string->number (list-ref ver 3))
               0))
          #f)))
  
  ;; version<= : string string -> boolean
  ;; determines if a is the version string of an earlier mzscheme release than b ;; [n.b. this relies on a guarantee from Matthew that mzscheme version
  ;; x1.y1 is older than version x2.y2 iff x1<x2 or x1=x2 and y1<y2]
  (define (version<= a b)
    (let ([a (string->version a)]
          [b (string->version b)])
      (or (<= (mz-version-major a) (mz-version-major b))
          (and (= (mz-version-major a) (mz-version-major b))
               (<= (mz-version-minor a) (mz-version-minor b)))))))