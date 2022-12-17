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
     (λ (n1 n2 n3 n4 n5 n6 n7)
       (define body
         (code
          (#,(fade-in-pointer (code syntax-parse) (* n4 (- 1 n6))) stx-obj
            #:literals (or)
            [(or e1:expr e2:expr)
             #'(#,(fade-in-pointer (code let) n6) ([x e1])
                 (#,(fade-in-pointer (code if) n6) x
                     x
                     e2))])))
       (define body1 (ghost (launder body)))
       (define body2
         (code
          (syntax-parse stx-obj
            #:literals (or)
            [(or e1:expr e2:expr)
             #'(let ([x e1])
                 (if x
                     x
                     e2))])))
       (define phase1
         (code
          (#,(become (code define) (code define-syntax) n1) (transform-or stx)
            #,body1)))
       (define phase2
         (code
          (define-syntax transform-or
            (λ (stx)
              #,body2))))

       (define words-on-side
         (vl-append
          (cellophane (vl-append
                       (t "compile-time:")
                       (hbl-append (tt "syntax/parse") (t " library")))
                      n5)
          (cellophane (vl-append
                       (t "run-time:")
                       (hbl-append (t "just the basics")))
                      n7)))
       
       (hc-append
        (lc-superimpose
         (blank 700 0) ;; this 700 should really be the full-formed width at n2
         (slide-pict
          (ltl-superimpose
           (cellophane phase1 (- 1 n3))
           (cellophane phase2 n3))
          body
          body1 body2 n2))
        words-on-side)))))

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
