#lang racket
(require "../lib/setup.rkt" slideshow)
#|


Need an example of a function that could plausibly
be called at compile time and at runtime.....


|#

(provide module-system)

(define (module-system)
  (with-title "The Module System"
    (slide (blank))))

(module+ main (module-system))
