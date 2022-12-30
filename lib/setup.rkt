#lang racket
(require slideshow/code slideshow racket/runtime-path)
(provide paper-images with-title)

(current-code-font '(bold . "Inconsolata"))
(current-tt-font '(bold . "Inconsolata"))
(current-tt-font-size (+ (current-font-size) 6))
(current-main-font "Lato")
(define-runtime-path paper-images "../paper-images")
(get-current-code-font-size (λ () (current-tt-font-size)))

(define-syntax-rule
  (with-title title e ...)
  (parameterize ([the-title title])
    e ...))
(define the-title (make-parameter #f))
(current-slide-assembler
 (let ([csa (current-slide-assembler)])
   (λ (_t vs content)
     (define title (the-title))
     (define titleless (csa _t vs content))
     (cond
       [title
        (define title-text-pict (colorize (t title) "white"))
        (define rect
          (colorize (filled-rounded-rectangle
                     (+ (pict-width title-text-pict) 40)
                     (+ (pict-height title-text-pict) 10))
                    "black"))
        (ct-superimpose
         (rt-superimpose (blank client-w 0) (inset (cc-superimpose rect title-text-pict) 0 0 40 0))
         titleless)]
       [else titleless]))))