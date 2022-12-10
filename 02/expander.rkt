#lang racket
(require "../lib/setup.rkt" slideshow slideshow/play slideshow/code)
(provide expander)

(define (expander)
  (with-title "The Expander"
    (slide
     (vl-append
      40
       
      (hbl-append (t "We need a driver loop; ")
                  (inset expand-p 8 0)
                  (t " does that job"))

      (vl-append
       (hbl-append expand-p (tt " : syntax-object (id → val) → syntax-object"))
       (t "the environment tracks the macros in scope"))
      
      (t "There are 3 interesting cases")))

    (expand-cases)
    #;
    (slide
     (vl-append
      (expand-call (code #'(if e1 e2 e3)))
      (t "=")
      (code #'(if e1′ e2′ e3′))
      (blank 0 80)
      (t "where")
      (ht-append (blank 20 0)
                 (vl-append
                  (hbl-append (code e1′) (t " = ") (expand-call (code e1)))
                  (hbl-append (code e2′) (t " = ") (expand-call (code e2)))
                  (hbl-append (code e3′) (t " = ") (expand-call (code e3)))))))))

(define (expand-cases)
  (define case1
    (vl-append
     (expand-call (code #'(if e1 e2 e3)))
     (t "=")
     (code #'(if e1′ e2′ e3′))))
  (define case2
    (vl-append
     (expand-call (code #'(m e1 ...)))
     (t "=")
     (expand-call (code (λ (stx) e2) #'(m e1 ...)))))
  (define case3
    (vl-append
     (expand-call (code #'(let-syntax ([id proc-e]) body-e)))
     (t "=")
     (expand-call (code body-e) (hbl-append (t "Γ + { ") (code id) (t " → ")
                                            (code (eval #'proc-e))
                                            (t " }")))))

  (define where1
    (vl-append
     (hbl-append (code e1′) (t " = ") (expand-call (code e1)))
     (hbl-append (code e2′) (t " = ") (expand-call (code e2)))
     (hbl-append (code e3′) (t " = ") (expand-call (code e3)))))

  (define where2
    (hbl-append (t "Γ(") (code m) (t ") = ") (code (λ (stx) e2))))
     
  (play-n
   (λ (n1 n2)
     (define (show p phase)
       (cellophane
        p
        (case phase
          [(1) (- 1 n1)]
          [(2) (* n1 (- 1 n2))]
          [(3) (* n1 n2)])))
     (vl-append

      (lt-superimpose (show (t "Case 1: found the IR") 1)
                      (show (t "Case 2: found a macro") 2)
                      (show (t "Case 3: found a macro definition") 3))
      (hc-append
       (blank 40 0)
       (lt-superimpose
        (show case1 1)
        (show case2 2)
        (show case3 3)))

      (blank 0 40)
      (cellophane (t "where") (- 1 n2))

      (hc-append
       (blank 40 0)
       
       (lt-superimpose
        (show where1 1)
        (show where2 2)))))))

   
(module+ main (expander))

(define expand-p
  (parameterize (#;[current-main-font "Zapfino"])
    (tt "Expand")))

(define (expand-call p [Γ (t "Γ")])
  (htl-append expand-p (t "⟦ ") p (t " , ") Γ (t " ⟧")))
