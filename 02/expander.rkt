#lang racket
(require "../lib/setup.rkt" "../lib/util.rkt" "../stolen-from-mflatt/code.rkt"
         slideshow slideshow/play slideshow/code)
(provide expander expand-case-2-reminder)

(define (expand-case-2-reminder)
  ((expand-cases-proc) 1 1 1 1 0))

(define (expander)
  (with-title "The Expander"
    (slide
     (vl-append
      80

      (table 3
             (list expand-p (tt ":") (tt "high-level-program")
                   (blank)  (blank)  (tt "(id → val)")
                   (blank)  (tt "→") (tt "program-in-the-ir"))
             (list rbl-superimpose rbl-superimpose lbl-superimpose)
             (list rbl-superimpose rbl-superimpose lbl-superimpose)
             10
             0)

      (t "there are 3 interesting cases")))

    (play-n (expand-cases-proc))))

(define (expand-cases-proc)
  (define (case1 n2a)
    (htl-append-with-bar
     #:gap 50
     #:bar-cellophane n2a
     (vl-append
      (expand-call (code (m e ...)))
      (t "=")
      (expand-call (f-call (code (m e ...)))))
     (cellophane
      (vl-append
       (expand-call (code m))
       (t "=")
       (expand-call (f-call (code m)))) n2a)))
  (define (case2 n)
    (vl-append
     (expand-call (code (let-syntax ([id proc-e]) body-e)))
     (t "=")
     (expand-call (code body-e)
                  (hbl-append (t "Γ + { ") (code id) (t " → ")
                              (eval-call (code proc-e) n)
                              (t " }")))))
  (define case3
    (vl-append
     (expand-call (code (if e1 e2 e3)))
     (t "=")
     (code (if e1′ e2′ e3′))))

  (define (where1 n1d)
    (vl-append
     40
     (hbl-append (t "Γ(") (code m) (t ") = ") f-p)
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
                     (tt "local-expand") (t " to get IR from ") (it "inside") (t " a macro")))))
      n1d)))

  (define where3
    (vl-append
     (hbl-append (code e1′) (t " = ") (expand-call (code e1)))
     (hbl-append (code e2′) (t " = ") (expand-call (code e2)))
     (hbl-append (code e3′) (t " = ") (expand-call (code e3)))))

  (λ (n1a n1b n2a n2b n3)

    (define (show p phase)
      (cellophane
       p
       (case phase
         [(1) (- 1 n2a)]
         [(2) (* n2a (- 1 n3))]
         [(3) n3])))
    (vl-append

     (lt-superimpose (show (t "Case 1: found a macro") 1)
                     (show (t "Case 2: found a macro definition") 2)
                     (show (t "Case 3: found a core form") 3))

     (blank 0 30)

     (hc-append
      (blank 40 0)
      (lt-superimpose
       (show (case1 n1a) 1)
       (show (case2 n2b) 2)
       (show case3 3)))

     (blank 0 40)
     (cellophane (t "where") (if (= n3 0) (- 1 n2a) n3))

     (hc-append
      (blank 40 0)

      (lt-superimpose
       (show (where1 n1b) 1)
       (show where3 3))))))

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


(define-values (expand-p eval-p f-p)
  (parameterize ([current-main-font (check-font "Brush Script MT")]
                 [current-font-size 48])
    (values (t "Expand")
            (t "Eval")
            (t "F"))))

(define (expand-call p [Γ (t "Γ")])
  (htl-append expand-p (t "⟦ ") p (inset (t " , ") 10 0) Γ (t " ⟧")))

(define (f-call p)
  (htl-append (t " ") f-p (t "⟦ ") p (t " ⟧")))

(define (eval-call p [n 0])
  (htl-append (refocus (lb-superimpose (inset eval-p -4 0 0 -10)
                                       (colorize (cellophane arrow-with-dot-on-arrowhead n) "red"))
                       eval-p)
              (t "⟦ ") p (t " ⟧")))

(module+ main
  (slide (expand-case-2-reminder)))
