#lang racket/base
(require racket/draw
         racket/class)

(provide tweak)

(define (tweak c)
  (cond
   [(string? c)
    (tweak (make-object color% c))]
   [else
    (define r (send c red))
    (define g (send c green))
    (define b (send c blue))
    (define v 1.0)
    (define (i* a b) (inexact->exact (round (* a b))))
    (make-color (i* v r) (i* v g) (i* v b))]))
