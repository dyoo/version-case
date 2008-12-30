(module test-version-case mzscheme
  (require "version-case.ss"
           (lib "mred.ss" "mred"))
    
  ;; Small test code to see that we can write unit-dependent code
  ;; that still runs under both 360 and 369.
  
    
  (version-case
   [(version<= (version) "360")
    (printf "old unit code~n")
    (require (lib "tool.ss" "drscheme")
             (lib "unitsig.ss"))
    
    (define tool@
      (unit/sig drscheme:tool-exports^
        (import drscheme:tool^)
        (define (phase1)
          (message-box "phase1"))
        (define (phase2)
          (message-box "phase2"))))]
   
   [else
    (printf "new unit code~n")
    (require (lib "tool.ss" "drscheme")
             (lib "unit.ss"))
    (define-unit tool@
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      (define (phase1)
        (message-box "phase1"))
      (define (phase2)
        (message-box "phase2")))]))