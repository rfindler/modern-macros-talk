#lang racket

(provide
 (contract-out
  [interpolate
   (-> real? real? (real-in 0 1) real?)]))
(define (interpolate v1 v2 n) (+ v1 (* n (- v2 v1))))
