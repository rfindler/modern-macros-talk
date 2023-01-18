#lang racket

;; sum : list-of-number -> number
(define (sum l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) sum(rest l))]))
