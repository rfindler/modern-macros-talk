#lang racket
(require slideshow "../lib/setup.rkt")
(provide ide-cool-things)
(define (ide-cool-things)
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
          )))


(module+ main (ide-cool-things))
