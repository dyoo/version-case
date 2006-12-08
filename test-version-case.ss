(module test-version-case mzscheme
  (require "version-case.ss")
  
  (version-case
   [(version<= (version) "369")
    (printf "old unit-code~n")
    (require (lib "tool.ss" "drscheme")
             (lib "unitsig.ss"))]
   [else
    (printf "new unit-code~n")
    (require (lib "tool.ss" "drscheme")
             (lib "unit.ss"))]))