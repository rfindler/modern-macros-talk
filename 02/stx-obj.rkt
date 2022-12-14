#lang racket
(require "../lib/setup.rkt" "../lib/util.rkt" slideshow/play slideshow
         "../stolen-from-mflatt/scope.rkt"
         "../stolen-from-mflatt/code.rkt"
         slideshow/code)

;; syntax objects are trees;
;;   -- parens are the children
;;   -- but there is also information that covers scopes.
;;   -- this is an approximation to the data definiton:
;;     stx-obj = scopes × properties × tree
;;     tree    = (listof stx-obj) | bool | symbol | number | ...
;;   -- 
;;   -- working directly with the raw data structure is complex
;;      so there is a DSL that lets you operate on syntax objects via pattern matching
;;  -- segue into the expander (and how it puts scopes on?)

(provide stx-obj)

(define (stx-obj)
  (with-title "Syntax Objects: Representing Scope"
    (scope-slides #:just-or? #t))
  
  (with-title "Syntax Object Data Structure"
    (slide
     (table
      3
      (list (t "stx-obj") (t "=") (t "(setof scope) × properties × tree")
            (t "tree") (t "=") (t "(listof stx-obj)")
            (blank) (t "|") (t "bool")
            (blank) (t "|") (t "symbol")
            (blank) (t "|") (t "number")
            (blank) (t "|") (t "⋯")
            (t "scope") (t "=") (hbl-append (colored-box scope1)
                                            (t " | ")
                                            (colored-box scope2)
                                            (t " | ")
                                            (colored-box scope3)
                                            (t " | ")
                                            (colored-box scope4)
                                            (t " | ")
                                            (t "⋯")))
      (list* rbl-superimpose rbl-superimpose lbl-superimpose)
      rbl-superimpose
      10 0)
     (blank) (blank)
     (vc-append
      (t "Just a data structure:")
      (hbl-append (it "wrong")
                  (t " colors means ")
                  (it "wrong")
                  (t " scope")))))

  (with-title "Working with Syntax Objects"
    (pattern-match-and-construct)))

(define (pattern-match-and-construct)
  (define (go #:define [define-highlighted? #f]
              #:transform-or? [transform-or?-highlighted? #f]
              #:syntax-parse [syntax-parse-highlighted? #f]
              #:pattern [pattern-highlighted? #f]
              #:stx-obj [stx-obj-highlighted? #f]
              #:body [body-highlighted? #f]
              #:e1 [e1-highlighted? #f])

    (define highlighted-picts '())
    (define-syntax-rule
      (add-highlight highlighted . more)
      (let ([hp (add-highlight/proc (code . more) 'id highlighted)])
        (when highlighted (set! highlighted-picts (cons hp highlighted-picts)))
        hp))
    
    (define main
     (code
      (code:comment "transform-or : stx-obj -> stx-obj")
      (#,(add-highlight define-highlighted? define) (#,(add-highlight transform-or?-highlighted? transform-or stx-obj))
        (#,(add-highlight syntax-parse-highlighted? syntax-parse) #,(add-highlight stx-obj-highlighted? stx-obj)
          #,(add-highlight pattern-highlighted? #:literals (or))
          [#,(add-highlight pattern-highlighted? (or #,(add-highlight e1-highlighted? e1:expr) e2:expr))
           #,(add-highlight body-highlighted?
                            #'(let ([x #,(add-highlight e1-highlighted? e1)])
                                (if x
                                    x
                                    e2)))]))))
    (slide
     (cond
       [(null? highlighted-picts)
        main]
       [else
        (add-arrows (cellophane main 0.5)
                    highlighted-picts)])))

  (go)
  (go #:define #t)
  (go #:transform-or? #t)
  (go #:syntax-parse #t)
  (go #:stx-obj #t)
  (go #:pattern #t)
  (go #:body #t)
  (go #:e1 #t))

(define (add-arrows main arrows)
  (for/fold ([main main])
            ([arrow (in-list arrows)])
    (define-values (x y) (lt-find main arrow))
    (define one-line (code one-line))
    (define with-arrow
      (pin-over
       main
       x
       (+ y (pict-height (code x)))
       (colorize
        arrow-with-dot-on-arrowhead
        "red")))
    (pin-over
     with-arrow
     x y
     arrow)))

(define (add-highlight/proc as-code sym on?)
  (cond
    [(and on? #f)
     (define basic (colorize (tt (~s sym)) "white"))
     (define extra-below-space 8)
     (refocus (cc-superimpose
               (colorize (inset (filled-rounded-rectangle (+ 16 (pict-width basic))
                                                          (+ extra-below-space (pict-height basic)))
                                0 0 0 (- extra-below-space))
                         "black")
               basic)
              basic)]
    [else
     as-code]))


(define (colored-box color)
  (define sizer (t "xx"))
  (inset (colorize (filled-rounded-rectangle (pict-width sizer) (pict-width sizer)) color)
         0 0 0 (- (/ (pict-height sizer) 4))))

(module+ main (stx-obj))
