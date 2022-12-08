#lang racket
(require "../lib/setup.rkt" slideshow slideshow/play slideshow/code)

(define (expander)

  (play-n
   (λ (n)

     (vl-append
      40
      (vl-append
       10
       (tt "expand : syntax-object → syntax-object")
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
   (htl-append
    (htl-append expand-p
                (t "⟦")
                (hbl-append
                 (code (if e_1
                           e_2
                           e_3))
                 (t "⟧")))
    (t "    =    ")
    (hbl-append (code (if #,(expand-call (code e_1))
                          #,(expand-call (code e_2))
                          #,(expand-call (code e_3))))))))
   
(module+ main (expander))

(define expand-p
  (parameterize (#;[current-main-font "Zapfino"])
    (tt "Expand")))

(define (expand-call p)
  (htl-append expand-p (t "⟦") p (t "⟧")))
