#lang racket
(require "../setup.rkt" slideshow "open-compiler.rkt")

(define (introduction)
  (slide
   (scale (vc-append
           10
           (t "Modern Macros")
           (t "vs")
           (t "an Open Compiler"))
          2))
  (open-compiler))

(module+ main (introduction))

