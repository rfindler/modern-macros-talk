#lang racket
(require "../lib/setup.rkt" "../lib/util.rkt"
         "deepest-pkg-paths.rkt"
         slideshow slideshow/play slideshow/code)

(provide module-system)

(define (module-system)
  (with-title "The Module System"
    (let-stx-vs-def-stx)
    (dependencies-motivation)
    (tower-of-compile-time)
    (show-the-requires)))


(define (let-stx-vs-def-stx)
  (slide
   (vl-append
    10
    (t "Sidebar:")
    (hc-append (blank 30 0)
               (vl-append
                10
                (code (let-syntax ([id proc-e]) body-e))
                (t "vs")
                (code (define-syntax id proc-e)))))))

(define (dependencies-motivation)
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
        (#,(become (code define) (code define-syntax) n1) (transform-or stx-obj)
          #,body1)))
     (define phase2
       (code
        (define-syntax transform-or
          (λ (stx-obj)
            #,body2))))

     (define words-on-side-indent (blank 30 0))
     (define words-on-side
       (vl-append
        20
        (cellophane (vl-append
                     (t "compile-time:")
                     (hc-append words-on-side-indent
                                (hbl-append (tt "syntax/parse") (t " library"))))
                    n5)
        (cellophane (vl-append
                     (t "run-time:")
                     (hc-append words-on-side-indent
                                (hbl-append (t "just the basics"))))
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
      words-on-side))))

(define (tower-of-compile-time)
  (define pkgs (tower-of-compile-time-pkgs))
  (play-n
   (λ (n)
     (vl-append
      80
      (inset (colorize (t "Tower of compile times:") "red") -100 0 0 0)
      (apply
       vc-append
       (for/list ([i (in-naturals)]
                  [pkg+count (in-list pkgs)])
         (match-define (cons pkg count) pkg+count)
         (define words (become (tt pkg)
                               (if (= count 1)
                                   (tt pkg)
                                   (tt (~a pkg " (" count ")")))
                               n))
         (cc-superimpose
          words
          (linewidth
           6
           (frame
            (blank (+ (* client-w .3) (* i 60))
                   (* (pict-height words) 1.8)))))))))))

(define (show-the-requires)
  (play-n
   (λ (n1 n2)

     (define syntax-parse (code syntax-parse))
     (define syntax-parse-spot (launder (ghost syntax-parse)))
     (define syntax/parse (code (for-syntax syntax/parse racket/base)))
     (define syntax/parse-spot (launder (ghost syntax/parse)))

     (define let-code (code let))
     (define let-spot (launder (ghost let-code)))
     (define if-code (code if))
     (define if-spot (launder (ghost if-code)))
     (define racket/base (code racket/base))
     (define racket/base-spot (launder (ghost racket/base)))
     
     (define main
       (vl-append
        (tt "#lang racket/base")
        (code
         code:blank
         (require #,syntax/parse-spot
                  #,racket/base-spot)
         code:blank
         (define-syntax (transform-or stx)
           (#,syntax-parse-spot stx-obj
             #:literals (or)
             [(or e1:expr e2:expr)
              #'(#,let-spot ([x e1])
                  (#,if-spot x
                      x
                      e2))])))))

     (define racket-base-fade-n (interpolate 1 .5 n1))
     
     (pin-over
      (pin-over
       (pin-over
        (pin-over
         (pin-over
          (cellophane main (interpolate 1 .5 n1))
          racket/base-spot
          lt-find
          (fade-in-pointer (cellophane racket/base (* (interpolate 1 .5 n1)
                                                      (interpolate 1 2 n2)))
                           n2))
         if-spot
         lt-find
         (fade-in-pointer (cellophane if-code (* (interpolate 1 .5 n1)
                                                 (interpolate 1 2 n2)))
                          n2))
        let-spot
        lt-find
        (fade-in-pointer (cellophane let-code (* (interpolate 1 .5 n1)
                                                 (interpolate 1 2 n2)))
                         n2))
       syntax/parse-spot
       lt-find
       (fade-in-pointer (cellophane syntax/parse (interpolate 1 .5 n2))
                        (* n1 (- 1 n2))))
      syntax-parse-spot
      lt-find
      (fade-in-pointer (cellophane syntax-parse (interpolate 1 .5 n2)) (* n1 (- 1 n2)))))))

(module+ main (module-system))
