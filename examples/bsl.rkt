#lang htdp/bsl

;; len : list-of-anything -> number
(define (howmany my-list)
  (cond
    [(empty? my-list) empty]
    [else (+ 1 (howmany (rest my-list)))]))
