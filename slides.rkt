#lang racket
(require "lib/setup.rkt"
         "01/01.rkt"
         "02/stx-obj.rkt"
         "02/expander.rkt"
         "02/module.rkt"
         "03/existing-cool-things.rkt"
         "04/ide-cool-things.rkt"
         "05/thanks.rkt"
         slideshow)

(introduction)

(slide
 (vl-append
  10
  (t "Three important pieces:")
  (t "   • Syntax Objects: data structure representing the AST")
  (t "   • The Expander: how we stitch the pieces together")
  (t "   • The Module System: how we find the pieces")))

(stx-obj)
(expander)
(module-system)

(slide
 (vl-append
  (vl-append
   20
   (t "Recap:")
   (t "   • Syntax Objects: data structure representing the AST")
   (t "   • The Expander: how we stitch the pieces together")
   (t "   • The Module System: how we find the pieces"))

  (blank 0 100)
 
  (t "Now: what can we do with all this?")))

(existing-cool-things)
(ide-cool-things)
(thanks)
(printf "~a milliseconds to build the slideshow\n" (current-process-milliseconds))
