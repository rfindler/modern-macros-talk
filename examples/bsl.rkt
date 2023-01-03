#lang htdp/bsl

;; len : list-of-anything -> number
(define (len l)
  (cond
    [(empty? l) empty]
    [else (+ 1 (len (rest l)))]))
