#lang racket
(require "../lib/setup.rkt" "../lib/util.rkt" "../stolen-from-mflatt/code.rkt"
         slideshow slideshow/play slideshow/code)
(provide expander expand-case-2-reminder)

(define (expand-case-2-reminder)
  ((expand-cases-proc #t) 1 1 1 1 0))

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

    (play-n (expand-cases-proc #f))))

(define (expand-cases-proc revising-for-eval?)
  (define (combine-case lhs rhs) (vl-append -10 lhs (t "=") rhs))
  (define case1a
    (combine-case
     (expand-call (code (m e ...)))
     (expand-call (f-call (code (m e ...))))))
  (define case1b
    (combine-case
     (expand-call (code m))
     (expand-call (f-call (code m)))))
  (define (case2 n)
    (combine-case
     (expand-call (code (let-syntax ([id proc-e]) body-e)))
     (expand-call (code body-e)
                  (hbl-append (t "Γ + { ") (code id) (t " → ")
                              (eval-call (code proc-e) n)
                              (t " }")))))
  (define case3
    (combine-case
     (expand-call (code (if e1 e2 e3)))
     (code (if e1′ e2′ e3′))))

  (define where1a 
    (hbl-append (t "Γ(") (code m) (t ") = ") f-p))

  (define where1b
    (add-box-around
     (vl-append
      4
      (t "Facilitating macro cooperation:")
      (hc-append
       (blank 20 0)
       (vl-append
        8
        (vl-append (t "• Introspect on Γ with")
                   (hbl-append (ghost (t "• ")) (tt "syntax-local-value")))
        (vl-append (hbl-append (t "• Use ")
                               (tt "local-expand")
                               (t " to get"))
                   (hbl-append (ghost (t "• "))
                               (t " IR from ")
                               (it "inside")
                               (t " a macro"))))))))

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


    (define where (t "where"))

    (vc-append
     (lt-superimpose
     
      (show
       (vl-append
        (t "Case 1: found a macro")
        (blank 0 30)
        (ht-append 100
                   case1a
                   (vl-append where
                              where1a))
        (blank 0 30)
        (htl-append 100
                    (cellophane case1b n1a)
                    (cellophane where1b n1b)))
       1)

      (vl-append
       (show (if revising-for-eval?
                 (colorize (t "So: what about that eval?") "red")
                 (t "Case 2: found a macro definition"))
             2)
       (blank 0 30)
       (show (case2 n2b) 2))

      (vl-append
       (show (t "Case 3: found a core form") 3)
       (blank 0 30)
       (show case3 3)
       (blank 0 40)
       (show where 3)
       (hc-append (blank 40 0) (show where3 3))))
     (blank 0 100))))

(define (add-box-around p)
  (refocus
   (cc-superimpose
    p
    (colorize (linewidth 4 (frame (ghost (launder (inset p 20))))) "red"))
   p))

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
  (expander)
  (slide (expand-case-2-reminder)))
