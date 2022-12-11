#lang racket
(require "../lib/setup.rkt" "../stolen-from-mflatt/code.rkt"
         slideshow slideshow/play slideshow/code)
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
     (expand-call (code #'(if e1 e2 e2)))
     (t "=")
     (code #'(if e1′ e2′ e3′))))
  (define (case1b n)
    (vl-append
     (expand-call (code #'(let ([x e1]) e2)))
     (t "=")
     (code #'(let ([#,(add-a-scope (code x) n) e1′] #,(add-a-scope (code e2′) n))))))
  (define case2
    (vl-append
     (expand-call (code #'(m e1 ...)))
     (t "=")
     (expand-call (code ((λ (stx) e2) #'(m e1 ...))))))
  (define case3
    (vl-append
     (expand-call (code #'(let-syntax ([id proc-e]) body-e)))
     (t "=")
     (expand-call (code #'body-e)
                  (hbl-append (t "Γ + { ") (code #'id) (t " → ")
                              (code (eval #'proc-e))
                              (t " }")))))

  (define where2
    (vl-append
     (hbl-append (code e1′) (t " = ") (expand-call (code e1)))
     (hbl-append (code e2′) (t " = ") (expand-call (code e2)))))

  (define where1
    (vl-append
     where2
     (hbl-append (code e3′) (t " = ") (expand-call (code e3)))))

  (define (inset/2-right p)
    (inset p
           0 0
           (- (/ (pict-width p) 2))
           0))
  (define before/after
    (htl-append
     40
     (inset/2-right (get-just-the-or-expansion-before-pict))
     (inset/2-right (get-just-the-or-expansion-after-pict))))
  
  (define (where3 n)
    (vl-append
     40
     (hbl-append (t "Γ(") (code m) (t ") = ") (code (λ (stx) e2)))
     (cellophane
      (hc-append
       40
       (colorize (vl-append
                  (t "add a scope to")
                  (hbl-append
                   (blank 30 0)
                   (vl-append
                    (t "everything in the")
                    (t "output of expansion,")
                    (t "but that is not in the")
                    (t "input"))))
                 "red")
       before/after)
      n)))
     
  (play-n
   (λ (n1 n2b n2 n3b n3)
     
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
                      (show (t "Case 1: found a core form that binds a variable") 2)
                      (show (t "Case 2: found a macro") 3)
                      (show (t "Case 3: found a macro definition") 4))

      (blank 0 30)
      
      (hc-append
       (blank 40 0)
       (lt-superimpose
        (show case1 1)
        (hbl-append (show (case1b n2b) 2)
                    (cellophane (colorize (t "add a scope") "red")
                                (* n2b (- 1 n2))))
        (show case2 3)
        (show case3 4)))

      (blank 0 40)
      (cellophane (t "where") (- 1 n3))

      (hc-append
       (blank 40 0)
       
       (lt-superimpose
        (show where1 1)
        (show where2 2)
        (show (where3 n3b) 3)))))))

(define (add-a-scope p n)
  (define w (pict-width p))
  (define h (pict-height (hbl-append p (t "e2′"))))
  (refocus (cc-superimpose
            (cellophane (colorize (filled-rounded-rectangle (+ w 4) (+ h 4)) scope2)
                        n)
            p)
           p))

   
(module+ main (expander))

(define expand-p
  (parameterize (#;[current-tt-font "Zapfino"])
    (tt "Expand")))

(define (expand-call p [Γ (t "Γ")])
  (htl-append expand-p (t "⟦ ") p (t " , ") Γ (t " ⟧")))

