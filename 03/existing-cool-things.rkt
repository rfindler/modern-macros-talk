#lang racket
(require slideshow slideshow/code slideshow/play
         "../lib/setup.rkt" "../lib/util.rkt")
(provide existing-cool-things)

(define (existing-cool-things)
  (with-title "Macro-defining macros"
    (play-n
     (λ (n1 n2 n3 n4)
       (vl-append
        60
        (lbl-stages
         1
         (t "Powerful idea: expand into a macro definition")
         n1
         (hbl-append
          (tt "define ") (t "from")
          (tt " htdp/bsl ") (t "turns into")
          (tt " define ") (t "from") (tt " racket/base"))
         n3
         (hbl-append (t "two cases in") (tt " syntax-parse") (t ", checked in order"))
         n4
         (t "raises an error, without returning a syntax object")
         0)
        (vl-append
         20
         (code (#,(fade-in-pointer (code define) (* n1 (- 1 n2))) (#,(fade-in-pointer (code f) (* n2 (- 1 n3))) x y z) (+ x y z)))
         (t "transforms into")
         (code
          (#,(fade-in-pointer (code define) (* n1 (- 1 n2))) (actual-f x y z) (+ x y z)))
         (cellophane
          (code
           (define-syntax (#,(fade-in-pointer (code f) (* n2 (- 1 n3))) stx)
             (syntax-parse stx
               #,(fade-in-pointer (code [x:id
                                         #,(fade-in-pointer (code (raise-syntax-error 'f "missing open paren" stx)) n4)])
                                  (* n3 (- 1 n4))
                                  #:superimpose ltl-superimpose)
               #,(fade-in-pointer (code [(_ arg:expr ...)
                                         #'(actual-f arg ...)])
                                  (* n3 (- 1 n4))
                                  #:superimpose ltl-superimpose))))
          (* (interpolate 1 .5 n1)
             (interpolate 1 2 n2)))))))))


(define (other-order-superimpose p1 p2)
  (refocus (lbl-superimpose p1 p2) p1))

(module+ main (existing-cool-things))
