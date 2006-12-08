(module test-version-case mzscheme
  (require "version-case.ss"
           (lib "mred.ss" "mred"))
  
  (version-case
   [(version<= (version) "369")
    (printf "old unit code~n")
    (require (lib "tool.ss" "drscheme")
             (lib "unitsig.ss"))
    
    (define my-tool
      (unit/sig drscheme:tool-exports^
        (import drscheme:tool^)
        (define (phase1)
          (message-box "phase1"))
        (define (phase2)
          (message-box "phase2"))))
    
    (define tool@
      (compound-unit/sig
        (import (DRSCHEME-TOOL : drscheme:tool^))
        (link (TOOL : drscheme:tool-exports^ (my-tool DRSCHEME-TOOL)))
        (export (open TOOL))))]
   
   [else
    (printf "new unit code~n")
    (require (lib "tool.ss" "drscheme")
             (lib "unit.ss"))])
  
  )