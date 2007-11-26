(module version-misc mzscheme
  (require (lib "string.ss")
           (lib "list.ss")
           (prefix 1: (lib "1.ss" "srfi")))
  
  (provide version<=)
  
  
  ;; The definitions of mz-version, string->version, and
  ;; version<= were adapted (copied and pasted) from PLaneT's
  ;; implementation in (planet/private/planet-shared.ss).
  
  (define-struct mz-version (major minor maintenances) #f)
  
  ;; string->version : string -> mz-version | #f
  (define (string->version str)
    (cond
      ;; Old style numbering
      [(regexp-match #rx"^([0-9][0-9][0-9])([.0-9]*)$" str)
       =>
       (lambda (ver)
         (let* ([major (string->number (list-ref ver 1))]
                [after-major
                 (map string->number
                      (rest (regexp-split "\\." (list-ref ver 2))))]
                [minor (if (>= (length after-major) 1)
                           (first after-major)
                           0)]
                [maintenances (drop after-major 1)])
           (make-mz-version major
                            minor
                            maintenances)))]
      
      ;; New style numbering
      [(regexp-match #rx"^([0-9])([.0-9]*)$" str)
       =>
       (lambda (ver)
         (let* ([major (string->number (list-ref ver 1))]
                [after-major
                 (map string->number
                      (rest (regexp-split "\\." (list-ref ver 2))))]
                [sub-major (if (>= (length after-major) 1)
                           (first after-major)
                           0)]
                [minor (if (> (length (drop after-major 1)) 0)
                           (first (drop after-major 1))
                           0)]
                [maintenances (drop after-major 2)])
           (make-mz-version (+ (* 100 major) sub-major)
                            minor
                            maintenances)))]
      [else #f]))
  
  
  ;; drop: (listof X) number -> (listof X)
  ;; A more permissive version of drop that returns the empty list
  ;; if we try to take off too many elements.
  (define (drop a-list n)
    (1:drop a-list (min n (length a-list))))
  
  
  ;; version<= : string string -> boolean
  ;; determines if a is the version string of an earlier
  ;; mzscheme release than b
  ;; [n.b. this relies on a guarantee from Matthew that
  ;; mzscheme version x1.y1 is older than version x2.y2 iff
  ;;  x1<x2 or x1=x2 and y1<y2]
  ;;
  ;; WARNING: currently, we do not discriminate with
  ;; maintenance numbers.
  (define (version<= a b)
    (let ([a (string->version a)]
          [b (string->version b)])
      (or (<= (mz-version-major a) (mz-version-major b))
          (and (= (mz-version-major a) (mz-version-major b))
               (<= (mz-version-minor a) (mz-version-minor b)))))))