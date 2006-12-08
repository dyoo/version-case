(module test-version-case mzscheme
  (require "version-case.ss")
  
  (version-case
   [(version<= (string->version (version))
               (string->version "369"))
    (printf "old unit-code~n")
    (require (lib "unitsig.ss"))]
   [else
    (printf "new unit-code~n")
    (require (lib "unit.ss"))]))