#lang racket
(require slideshow "../lib/setup.rkt")
(provide ide-cool-things)

#;
(slide (para
        #<<--

need an introduction-- how can we leverage an open compiler and get an open IDE?

IDE cool things:
  
  -- arrows ----> simple: just track binding.
     Complex: redex
  -- something in the contract system?  ... blame annotations
      --> TR shows you the types
  -- keystrokes; enforce the language of the module!

also: what things do I need to foreshadow here?

--
        ))

(define (ide-cool-things)
  
  (slide
   (vl-append
    (colorize (t "from:") "red")
    (scale (t "an Open Compiler") 2)
    (blank 0 60)
    (colorize (t "to:") "red")
    (scale (t "a Configurable IDE") 2))))


(module+ main (ide-cool-things))
