#lang racket
(require racket/draw
         pict
         "to-outlines.rkt")

(provide pict->offset-cards
         (struct-out card))

(define card-cache (make-hash))

(struct card (name datum dc-path red green blue))

(define (pict->offset-cards p)
  (define dc (new record-dc%))
  (draw-pict p dc 0 0)
  (define outlines (record-dc->outlines dc))

  (define (outline->offset-card o)
    (define p (outline-path o))
    (define-values (x1 y1 x2 y2) (send p get-bounding-box))
    (send p translate (- x1) (- y1))
    (define-values (c ignored) (send p get-datum))
    (define col (outline-color o))
    (define crd (hash-ref! card-cache
                           (list c (send col red) (send col green) (send col blue))
                           (lambda ()
                             (card (gensym) c p (send col red) (send col green) (send col blue)))))
    (vector crd x1 y1))
  
  (map outline->offset-card outlines))
