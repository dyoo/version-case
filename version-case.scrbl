#lang scribble/doc
@(require scribble/manual
          (for-label scheme)
          (for-label "version-misc.ss")
          (for-label "version-case.ss"))

@title{version-case: conditionally compile code based on current version number}
                

This library provides support for conditionally compiling code based
on the version of PLT Scheme.  One common application of this module
is to write compatibility bindings.



@section{Example}

The following example shows how one can write unit code that works
with both the old and new @scheme[unit] libraries.


@schemeblock[
(module some-sample-unit-code mzscheme
  (require (planet "version-case.ss" ("dyoo" "version-case.plt" 1))
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
        (message-box "phase2")))]))]
                

Another simple example:
@schemeblock[
(module another-example scheme/base
  (require (planet dyoo/version-case)
           (for-syntax scheme/base))
  (printf "~a~n" (version-case [(version<= (version) "4")
                                'huh?]
                               [else
                                'ok])))]


@section{Usage}

@schemeblock[
(version-case [test code ...]
              ... 
              [else code ...])
]

@scheme[version-case] is a macro that expands out to one of the
@scheme[code] blocks, depending on which test succeeds first.  The
@scheme[test] expression is evaluated at compile-time.  Some
version-comparing functions are available for convenience.

@declare-exporting[version-case/version-misc]
@defproc[(version< [v1 string?] [v2 string?]) boolean?]{
Returns true if @scheme[v1] is less than @scheme[v2].
}
@defproc[(version<= [v1 string?] [v2 string?]) boolean?]{
Returns true if @scheme[v1] is less than or equal to @scheme[v2].
}
@defproc[(version= [v1 string?] [v2 string?]) boolean?]{
Returns true if @scheme[v1] is equal to @scheme[v2].
}
@defproc[(version> [v1 string?] [v2 string?]) boolean?]{
Returns true if @scheme[v1] is greater than @scheme[v2].
}
@defproc[(version>= [v1 string?] [v2 string?]) boolean?]{
Returns true if @scheme[v1] is greater than or equal to @scheme[v2].
}





@section{Gotchas}

The tests are done at compile time.  If the language of your module
doesn't include compile-time bindings for function application, you
may see funny error messages.  For example, @scheme[scheme/base]
doesn't automatically provide the necessary compile-time bindings, so
if you use @scheme[version-case] with it, you also need to do a
@scheme[(require (for-syntax scheme/base))].



@section{Thanks}

Special thanks to Carl Eastlund providing the implementation that
doesn't use @scheme[eval], and for feedback.  Thanks also to Ambjorn
Elder for noticing a bug regarding the use of syntax-case within a
version-case's body.
