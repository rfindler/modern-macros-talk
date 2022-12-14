#lang racket
(require slideshow pict "setup.rkt")

(provide
 (contract-out
  [interpolate
   (-> real? real? (real-in 0 1) real?)]
  [arrow-with-dot-on-arrowhead pict?]))
(define (interpolate v1 v2 n) (+ v1 (* n (- v2 v1))))

(define arrow-with-dot-on-arrowhead
  (let ()
    (define base (scale (t "➹") 4)) ;➷
    (define spot (blank))
    (refocus (pin-over
              base
              (* (pict-width base) 9/10)
              (* (pict-height base) 1/4)
              spot)
             spot)))