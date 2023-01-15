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

(introduction)
(expander)
(with-title "Syntax Objects: Representing Scope" (open-compiler-part2))
(stx-obj)
(module-system)

(with-title "Recap"
  (slide
   (vl-append
    (vl-append
     20
     (t "Recap:")
     (t "   • The Expander: a driver loop for front-end compilation")
     (t "   • Syntax Objects: data structure representing the AST")
     (hbl-append (t "   • The Module System: dependencies across compile")
                 (parameterize ([current-main-font (cons 'superscript (current-main-font))])
                   (t "n"))
                 (t " times")))

    (blank 0 100)
 
    (t "Now: what can we do with all this?"))))

(existing-cool-things)
(ide-cool-things)
(thanks)
(printf "~a milliseconds to build the slideshow\n" (current-process-milliseconds))
