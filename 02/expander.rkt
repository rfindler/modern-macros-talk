#lang racket
(require "../lib/setup.rkt" slideshow slideshow/play slideshow/code)
(provide expander)

(define (expander)
  (with-title "The Expander"
    (play-n
     (λ (n)

       (vl-append
        40
        (vl-append
         10
         (tt "expand : syntax-object Γ → syntax-object")
         (hbl-append (t "We need an open driver loop; ")
                     (inset (tt "expand") 8 0)
                     (t " does that job")))
  

        (cellophane
         (vl-append
          10
          (t "Three interesting cases:")
          (hc-append (blank 40 0)
                     (vl-append
                      10
                      (t "• a core form")
                      (t "• a use of a macro")
                      (t "• a definition of a macro"))))
         n))))


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

#|

(expand (or (f x) (g y)))
=
(expand (or-proc #'(or (f x) (g y)))

where

(define-syntax or (λ (stx) ...))


(expand Γ (define-syntax id proc) ...)
=
(expand Γ+{id→proc} ...)

where ... is what follows in the scope of the define-syntax.

|#
   
(module+ main (expander))

(define expand-p
  (parameterize (#;[current-main-font "Zapfino"])
    (tt "Expand")))

(define (expand-call p)
  (htl-append expand-p (t "⟦ ") p (t " , Γ") (t " ⟧")))
