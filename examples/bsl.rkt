#lang htdp/bsl

(define (len l)
  (cond
    [(empty? l) empty]
    [else (+ 1 (len (rest l)))]))
