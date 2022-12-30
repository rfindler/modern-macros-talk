#lang racket
(require slideshow pict "setup.rkt")

(provide
 (contract-out
  [become (-> pict? pict? (real-in 0 1) pict?)]
  [fade-in-pointer (->* (pict? (real-in 0 1)) (#:superimpose any/c) pict?)]
  [interpolate (-> real? real? (real-in 0 1) real?)]
  [arrow-with-dot-on-arrowhead pict?])
 lbl-stages)
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

(define (fade-in-pointer p n #:superimpose [superimpose lbl-superimpose])
  (refocus
   (superimpose p (cellophane (colorize arrow-with-dot-on-arrowhead "red") n))
   p))

(define (become p1 p2 n)
  (define m (max (pict-width p1) (pict-width p2)))
  (define sized (blank (interpolate (pict-width p1) (pict-width p2) n)
                       (interpolate (pict-height p1) (pict-height p2) n)
                       (interpolate (pict-ascent p1) (pict-ascent p2) n)
                       (interpolate (pict-descent p1) (pict-descent p2) n)))
  (clip (refocus (lbl-superimpose
                  sized
                  (cellophane p1 (- 1 n))
                  (cellophane p2 n))
                 sized)))

;; shows the picts in its arguments one after the other,
;; combined with lbl-superimpose, fading from one to the next
(define (lbl-stages . ns-and-ps)
  (define number-of-args (length ns-and-ps))
  (unless (odd? number-of-args)
    (error 'lbl-stages "expected an odd number of arguments, got ~a arguments"
           number-of-args))
  (unless (>= number-of-args 3)
    (error 'lbl-stages "expected at least three arguments, got ~a arguments"
           number-of-args))
  (for ([arg (in-list ns-and-ps)]
        [i (in-naturals)])
    (define p? (if (even? i) real? pict?))
    (unless (p? arg)
      (apply raise-argument-error
             'lbl-stages (symbol->string (object-name p?)) i
             ns-and-ps)))
  (let loop ([before (list-ref ns-and-ps 0)]
             [p (list-ref ns-and-ps 1)]
             [after (list-ref ns-and-ps 2)]
             [more (drop ns-and-ps 3)])
    (define this-one
      (cellophane p (* before (- 1 after))))
    (cond
      [(null? more) this-one]
      [else
       (lbl-superimpose
        this-one
        (loop after (list-ref more 0) (list-ref more 1) (drop more 2)))])))