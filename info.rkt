#lang info

(define collection "modern-macros-talk")
(define deps '("base"
               "draw-lib" "gui-lib" "pict-lib" "slideshow-lib"
               "drracket-tool-lib"
               "reactor" "datalog"
               "recursive-language" "redex-gui-lib"  "dssl2"
               "turnstile-lib" "typed-racket-lib"))
(define build-deps '("scribble-lib"))
(define pkg-desc "Implementation and abstract of a talk given at PADL 2023")
(define license '(Apache-2.0 OR MIT))
(define scribblings (list (list "modern-macros-talk.scrbl")))
