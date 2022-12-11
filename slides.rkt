#lang racket
(require "lib/setup.rkt"
         "01/01.rkt"
         "02/stx-obj.rkt"
         "02/expander.rkt"
         "02/module.rkt"
         "03/enum.rkt"
         slideshow)

(introduction)

#|

Need to introduce the idea that we discover the
scope only through the expansion process!

|#

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

(enum-slides)
