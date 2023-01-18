#lang racket
(require "private/setup.rkt"
         "private/introduction.rkt"
         "private/open-compiler.rkt"
         "private/stx-obj.rkt"
         "private/expander.rkt"
         "private/module.rkt"
         "private/existing-cool-things.rkt"
         "private/ide-cool-things.rkt"
         "private/thanks.rkt"
         slideshow)

(introduction)
(expander)
(with-title "Syntax Objects: Representing Scope" (open-compiler-part2))
(stx-obj)
(module-system)

(with-title "Recap: the essentials of a modern macro system"
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
