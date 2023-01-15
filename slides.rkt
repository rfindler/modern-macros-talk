#lang racket
(require "lib/setup.rkt"
         "01/01.rkt"
         "01/open-compiler.rkt"
         "02/stx-obj.rkt"
         "02/expander.rkt"
         "02/module.rkt"
         "03/existing-cool-things.rkt"
         "04/ide-cool-things.rkt"
         "05/thanks.rkt"
         slideshow)

#|

swap the expander to come before the syntax objects;
  -- move the "adding scopes" discussion in the expander out into the syntax object part
  --- bring in the "equivlance relation on identifiers" idea when looking at the
      compiler in the "or" case and set up some discussion to come later about
      equivalence relations on identifiers (need to think about this)


|#

(introduction)

(stx-obj)
(with-title "Breaking up is hard to do...." (open-compiler-part2))
(expander)
(module-system)

(with-title "Recap"
  (slide
   (vl-append
    (vl-append
     20
     (t "Recap:")
     (t "   • Syntax Objects: data structure representing the AST")
     (t "   • The Expander: how we stitch the pieces together")
     (t "   • The Module System: how we find the pieces"))

    (blank 0 100)
 
    (t "Now: what can we do with all this?"))))

(existing-cool-things)
(ide-cool-things)
(thanks)
(printf "~a milliseconds to build the slideshow\n" (current-process-milliseconds))
