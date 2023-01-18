#lang racket
(require slideshow "setup.rkt")
(provide ide-cool-things)

(define (ide-cool-things)
  
  (slide
   (vl-append
    (colorize (t "from:") "red")
    (scale (t "an Open Compiler") 2)
    (blank 0 60)
    (colorize (t "to:") "red")
    (scale (t "a Configurable IDE") 2))))


(module+ main (ide-cool-things))
