#lang racket
(require "../lib/setup.rkt" slideshow)
#|


Need an example of a function that could plausibly
be called at compile time and at runtime.....


|#

(provide module-system)

(define (module-system)
  (with-title "The Module System"
    (slide (blank))))

#|

0. introduce "terminology" with boxes and arrows for dependencies(?)

1. show a macro definition and discuss dependencies
   ----> or example from earlier; don't need syntax-parse at compile time
   ----> more archane point: the language you're compiling into might
         be different than the one you're compiling with

2. show some boxes and arrows diagrams with modules and dependencies
   based on real(ish) example

3. ...

|#

(module+ main (module-system))
