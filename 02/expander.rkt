#lang racket
(require "../lib/setup.rkt" "../lib/util.rkt" "../stolen-from-mflatt/code.rkt"
         slideshow slideshow/play slideshow/code)
(provide expander)

(define (expander)
  (with-title "The Expander"
    (slide
     (vl-append
      40

      (table 3
             (list expand-p (tt ":") (tt "syntax-object")
                   (blank)  (blank)  (tt "(id → val)")
                   (blank)  (tt "→") (tt "syntax-object"))
             (list rbl-superimpose rbl-superimpose lbl-superimpose)
             (list rbl-superimpose rbl-superimpose lbl-superimpose)
             10
             0)
      (t "the environment tracks the macros in scope")

      (t "there are 3 interesting cases")))

    (expand-cases)))

(define (expand-cases)
  (define case1
    (vl-append
     (expand-call (code #'(if e1 e2 e3)))
     (t "=")
     (code #'(if e1′ e2′ e3′))))
  (define (case1b n)
    (vl-append
     (expand-call (code #'(let ([x e1]) e2)))
     (t "=")
     (code #'(let ([#,(add-a-scope (code x) n) e1′] #,(add-a-scope (code e2′) n))))))
  (define (case2 n2a)
    (htl-append-with-bar
     #:gap 50
     #:bar-cellophane n2a
     (vl-append
      (expand-call (code #'(m e1 ...)))
      (t "=")
      (expand-call (code ((#,(t "λ") (stx) e2)
                          #'(m e1 ...)))))
     (cellophane
      (vl-append
       (expand-call (code #'m))
       (t "=")
       (expand-call (code ((#,(t "λ") (stx) e2)
                           #'m)))) n2a)))
  (define (case3 eval-p)
    (vl-append
     (expand-call (code #'(let-syntax ([id proc-e]) body-e)))
     (t "=")
     (expand-call (code #'body-e)
                  (hbl-append (t "Γ + { ") (code #'id) (t " → ")
                              (code (#,eval-p #'proc-e))
                              (t " }")))))

  (define where1b
    (vl-append
     (hbl-append (code e1′) (t " = ") (expand-call (code e1)))
     (hbl-append (code e2′) (t " = ") (expand-call (code e2)))))

  (define where1
    (vl-append
     where1b
     (hbl-append (code e3′) (t " = ") (expand-call (code e3)))))

  (define (inset/2-right p)
    (inset p
           0 0
           (- (/ (pict-width p) 2))
           0))

  (define the-before-pict (get-just-the-or-expansion-before-pict))
  (define the-after-pict (get-just-the-or-expansion-after-pict))

  (define (where2 n2b n2c n2d n2e)
    (vl-append
     40
     (hbl-append (t "Γ(") (code #'m) (t ") = ") (code (λ (stx) e2)))
     (lt-superimpose
      (cellophane
       (hc-append
        40
        (cellophane
         (colorize (vl-append
                    (t "add a fresh scope to")
                    (hbl-append
                     (blank 30 0)
                     (vl-append
                      (t "everything in the")
                      (t "output of expansion,")
                      (t "but that is not in the")
                      (t "input"))))
                   "red")
         n2c)
        (htl-append
         40
         (cellophane (inset/2-right the-before-pict) n2b)
         (cellophane (inset/2-right the-after-pict) n2c)))
       (- 1 n2d))
      (cellophane
       (vl-append
        4
        (blank 0 20)
        (t "Facilitating macro cooperation:")
        (hc-append
         (blank 20 0)
         (vl-append
          (hbl-append (t "• Introspect on Γ with ") (tt "syntax-local-value"))
          (hbl-append (t "• Use ")
                      (tt "local-expand") (t " to expand from ") (it "inside") (t " a macro")))))
       n2e))))

  (play-n
   (λ (#;n1 n1b n2 n2a n2b n2c n2d n2e n3 n3b)
     (define n1 1)
     ;(define n1b 1)
     (define plain-eval (code eval))
     (define eval-arrow
       (refocus (lb-superimpose plain-eval
                                (colorize (cellophane arrow-with-dot-on-arrowhead n3b) "red"))
                plain-eval))

     (define (show p phase)
       (cellophane
        p
        (case phase
          [(1) (- 1 n1)]
          [(2) (* n1 (- 1 n2))]
          [(3) (* n1 n2 (- 1 n3))]
          [(4) (* n1 n2 n3)])))
     (vl-append

      (lt-superimpose (show (t "Case 1: found a core form") 1)
                      (show (t "Case 1: found a core form") 2) ;; used to say "that binds a variable"
                      (show (t "Case 2: found a macro") 3)
                      (show (t "Case 3: found a macro definition") 4))

      (blank 0 30)

      (hc-append
       (blank 40 0)
       (lt-superimpose
        (show case1 1)
        (hbl-append (show (case1b n1b) 2)
                    (cellophane (colorize (t "add a fresh scope") "red")
                                (* n1b (- 1 n2))))
        (show (case2 n2a) 3)
        (show (case3 eval-arrow) 4)))

      (blank 0 40)
      (cellophane (t "where") (- 1 n3))

      (hc-append
       (blank 40 0)

       (lt-superimpose
        (show where1 1)
        (show where1b 2)
        (show (where2 n2b n2c n2d n2e) 3)))))))

(define (htl-append-with-bar #:gap [gap 0] #:bar-cellophane [bar-cellophane 1] p1 p2)
  (define without-bar (htl-append gap p1 p2))
  (define extra-v-space 10)
  (define bar (cellophane (frame (blank 0 (+ extra-v-space (pict-height without-bar) extra-v-space)))
                          bar-cellophane))
  (define-values (x y) (lt-find without-bar p2))
  (pin-over
   without-bar
   (- x (/ gap 2))
   (- extra-v-space)
   bar))

(define (add-a-scope p n)
  (define w (pict-width p))
  ;; non-general hack to get the heights the same....
  (define h (pict-height (t "e2′")))
  (refocus (cbl-superimpose
            ;;  the -10 is another non-general hack
            (cellophane (colorize (inset (filled-rounded-rectangle (+ w 4) (+ h 4)) 0 0 0 -10) scope2)
                        n)
            p)
           p))


(define expand-p
  (parameterize (#;[current-tt-font "Zapfino"])
    (tt "Expand")))

(define (expand-call p [Γ (t "Γ")])
  (htl-append expand-p (t "⟦ ") p (inset (t " , ") 10 0) Γ (t " ⟧")))

(module+ main (expander))
