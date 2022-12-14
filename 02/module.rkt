#lang racket
(require "../lib/setup.rkt" "../lib/util.rkt"
         slideshow slideshow/play slideshow/code)
#|


Need an example of a function that could plausibly
be called at compile time and at runtime.....


|#

(provide module-system)

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

(define (module-system)
  (with-title "The Module System"
    (define space (blank (* client-w 3/4) (* client-h 1/2)))
    (play-n
     (λ (n)
       (lc-superimpose
        space
        (code
         (#,(become (code define) (code define-syntax) n) (transform-or stx)
           (syntax-parse stx-obj
             #:literals (or)
             [(or e1:expr e2:expr)
              #'(let ([x e1])
                  (if x
                      x
                      e2))]))))))))

(define (become p1 p2 n)
  (define m (max (pict-width p1) (pict-width p2)))
  (define sized (blank (interpolate (pict-width p1) (pict-width p2) n)
                       (interpolate (pict-height p1) (pict-height p2) n)
                       (interpolate (pict-ascent p1) (pict-ascent p2) n)
                       (interpolate (pict-descent p1) (pict-descent p2) n)))
  (clip (refocus (lbl-superimpose
                  sized
                  (cellophane p1 (- 1 n))
                  (cellophane p2 n))
                 sized)))

(module+ main (module-system))
