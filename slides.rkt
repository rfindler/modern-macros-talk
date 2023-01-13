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

Need to extra desiderata from the `compiler` slide.

-- open recursion

-- need a way to name the cases to find them
    --> partly about the module system,
    --> partly just about distinguishing cases

-- need cooperation between the cases

-- we only find out about the compilation by doing it ....
   that's what happens when we are going to plug in the rules! (this works)

-- common datastructure (stx objects)

  -- need to handle temporary variables and avoid accidental capture
    --> but it is worse! What about the free variables
        (is that even explained in the current version?)

  -- need to support tearing apart and putting back syntax while keeping lexical information
  
  -- being able to get the binding arrows right after the transformations without knowing the binding structure beforehand

  -- need to support macro-defining macros..... does this come first?

------> something about the "find out what happneed by expansion" (only)?

-- how to motivate the `eval` in the macro definition; can we do that here?


============================================================

Idea: can I move the expander discussion first (motivate it with the front end slide)
      then show a macro-generating macro
      then explain the data structure?

    If so, where does the module system go?

    Do I show the full macro-generating macro? (hopefully not necessary)

    ack; the expander introduces scopes! ... can I revisit those slides later?

|#


(introduction)

(stx-obj)
(open-compiler-part2)
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
