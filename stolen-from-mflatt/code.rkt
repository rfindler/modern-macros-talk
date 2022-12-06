#lang racket/base
(require (for-syntax racket/base)
         pict
         pict/code
         racket/format
         racket/draw
         racket/class
         racket/math
         racket/list
         "script.rkt"
         "color.rkt")

(provide premade-or-example
         just-or-example
         basic-lexical-scope
         scopes-example
         
         premade-or-example-script
         encolors
         
         scope1 scope1/text
         scope2 scope2/text
         scope3
         scope4
         scope5
         scope6)

(define (current-font-size) 32)
(get-current-code-font-size current-font-size)
(define gap-size 24)

(define (t s)
  (text s '("Gill Sans" . swiss) (current-font-size)))
(define (it s)
  (text s '(italic "Gill Sans" . swiss) (current-font-size)))
(define (tt s)
  (text s (current-code-font) (current-font-size)))

(define (shift find dx dy)
  (lambda (p q)
    (define-values (x y) (find p q))
    (values (+ x dx) (+ y dy))))

;; Backgrounds:
(define scope1 (tweak "lightcoral")) ;; was "pink"
(define scope1/text (tweak scope1)) ;; was "hotpink"
(define scope2 (tweak "deepskyblue")) ;; was "lightblue"
(define scope2/text scope2)
(define scope3 (tweak "gold")) ;; was "orange"
(define scope4 (tweak (make-color 0 220 0))) ; matching introduce2-color
(define scope5 (tweak "orchid")) ; (make-color 182 109 255))
(define scope6 (tweak (make-color 182 109 0)))

(define module-scope scope2)
(define intro1-scope scope2)

; Foregrounds:
(define introduce1-color (make-color 219 109 0)) ; matching `intro1-scope`
(define introduce2-color "forestgreen") ; matching `scope4`

(define arrow-color "purple")

(define-syntax-rule (intro2 e)
  (colorize
   (parameterize ([code-colorize-enabled #f])
     (code e))
   introduce1-color))
(define-syntax-rule (green e)
  (colorize
   (parameterize ([code-colorize-enabled #f])
     (code e))
   introduce2-color))

(define one-blank (code code:blank))
(define two-blank (code code:blank
                        code:blank))

(define (pin-bind-line p f t
                       #:rev? [rev? #f]
                       #:color [color arrow-color])
  (define find-top (shift ct-find 0 (* 1/4 (pict-height f))))
  (define find-bottom (shift cb-find 0 (* -1/4 (pict-height t))))
  (pin-arrow-line (/ (current-font-size) 2)
                  p
                  f (if rev? find-bottom find-top)
                  t (if rev? find-top find-bottom)
                  #:color color
                  #:line-width 3))

(define (pad-use-of-or p)
  (lt-superimpose p
                  (let ([s (tt "x")])
                    (blank (* (pict-width s) 25)
                           (* (pict-width s) 12)))))

(define (basic-lexical-scope)
  (let* ([x1 (code x)]
         [y1 (code x)]
         [x2 (launder x1)]
         [y2 (launder y1)]
         [x3 (code y)]
         [y3 (intro2 y)]
         [x4 (launder x3)]
         [y4 (intro2 y)]
         [y5 (intro2 y)])
    (define (mk-p body post)
      (pad-use-of-or
       (code
        (define #,x1 1)
        #,(code
           code:blank
           (let ([#,y1 #,x2])
             (λ (#,x3)
               #,body))
           #,post))))
    (define p (mk-p (code (or #,x4 #,y2)) two-blank))
    (define p2 (mk-p (code (#,(intro2 let) ([#,y3 #,x4])
                            (#,(intro2 if) #,y4 #,y5 #,y2)))
                     one-blank))
    
    (define lexical-title "Lexical Scope")
    
    (define (bind-orig p)
      (let* ([p (pin-bind-line p x2 x1)]
             [p (pin-bind-line p x4 x3)]
             [p (pin-bind-line p y2 y1)])
        p))
    
    (define (bind-all p)
      (let* ([p (bind-orig p)]
             [p (pin-bind-line p y4 y3)]
             [p (pin-bind-line p y5 y3)])
        p))
    
    (list p
          (bind-orig p))))

(define (encolor col p
                 #:out [out 0]
                 #:fill? [fill? #f]
                 #:skip [skip 0])
  (define q (inset p (* 2 (+ 1 out))))
  (define h (pict-height q))
  (define w (pict-width q))
  (refocus
   (cc-superimpose
    (colorize ((if fill?
                   values
                   (lambda (p)
                     (linewidth 3 p)))
               (cond
                [(zero? skip)
                 ((if fill?
                      filled-rounded-rectangle
                      rounded-rectangle)
                  w h 5)]
                [else
                 (define p (new dc-path%))
                 (send p move-to 0 (* h skip))
                 (send p arc 0 (- h 10) 10 10 pi (* 3/2 pi))
                 (send p arc (- w 10) (- h 10) 10 10 (* 3/2 pi) (* 2 pi))
                 (send p line-to w (* h skip))
                 (send p close)
                 (dc (lambda (dc x y)
                       (send dc draw-path p x y))
                     w h)]))
              col)
    p)
   p))

(define (encolors p #:int? [int? #f] . cols)
  (define ps (for/list ([col (in-list cols)]
                        [i (in-naturals 0)])
               (inset (encolor #:fill? #t
                               #:skip (/ i (length cols))
                               col
                               (ghost p)) 3)))
  (refocus
   (cc-superimpose
    (for/fold ([p (first ps)]) ([q (in-list (cdr ps))])
      (cb-superimpose p q))
    p)
   p))

(define (mark p) (launder (ghost p)))

(define (premade-or-example-script paint?)
  (let* ([real-encolor encolor]
         [encolors (if paint? encolors (lambda (p . cols) p))]
         [encolor (if paint? encolor (lambda (c #:out [out 0] p) p))])
    (define bind-e (encolors (code e) module-scope))
    (define use-e (encolors (code e) module-scope))
    (define tmpl-or (encolors (code or) module-scope))
    (define tmpl-x (encolors (code x) module-scope))
    (define or-macro? (not paint?))
    (define or-defn
      (code (define-syntax-rule (or a b)
              (let ([x a])
                (if x x b)))))
    (define (colorize-comment p)
     (colorize p (current-base-color)))
    (define premade-defn
      (let-syntax ([define (make-code-transformer #'(encolors (code define) module-scope))]
                   [x (make-code-transformer #'(encolors (code x) module-scope))])
        (code (define x ....))))
    (define long-premade-defn
      (if paint?
          premade-defn
          (refocus (code (code:comment #,(colorize-comment (t "maybe value for “premade”:")))
                         #,premade-defn)
                   premade-defn)))
    (define (mk-p x/bind body body-mark [show-second values] [show-third values] [wrap-body values]
                  #:post-or [post-or #f]
                  #:premade-defn [premade-defn long-premade-defn])
      (let-syntax ([let (make-code-transformer #'(encolors (code let) module-scope))]
                   [define (make-code-transformer #'(encolors (code define) module-scope))]
                   [define-syntax-rule (make-code-transformer #'(encolors (code define-syntax-rule) module-scope))]
                   [premade-or (make-code-transformer #'(encolors (code premade-or) module-scope))]
                   [x (make-code-transformer #'(encolors (code x) module-scope))])
        (let-values ([(premade-or-pattern)
                      (let-values ([(p) (code (premade-or #,bind-e))])
                        (if body-mark
                            (refocus (vc-append 2 p body-mark)
                                     p)
                            p))]
                     [(premade-or-template)
                      (let-values ([(p) (code (#,tmpl-or #,tmpl-x #,use-e))])
                        (if post-or
                            (refocus (vl-append 2 p post-or)
                                     p)
                            p))])
          (encolor
           module-scope
           #:out 20
           (code
            #,premade-defn
            code:blank
            #,(show-second
               (code (define-syntax-rule #,premade-or-pattern
                       #,premade-or-template)))
            code:blank
            #,(show-third
               (code (let ([#,x/bind ....])
                       #,(wrap-body body)))))))))
    
    (define (locate m u x) 
      (define-values (dx dy) (lt-find u x))
      (pin-over m dx dy (ghost x))) ; so x is findable in the mark
    
    (define (mk-body . cols)
      (let ([ord (apply encolors (code premade-or) cols)]
            [x (apply encolors (code x) cols)])
        (define app-x (code (#,x)))
        (define app-x-mark (locate (mark app-x) app-x x))
        (define u (code (#,ord #,app-x-mark)))
        (define u-mark (locate (locate (mark u) u x) u app-x-mark))
        (define u-mark-1 (mark u))
        (values x 
                app-x app-x-mark
                u u-mark u-mark-1
                (mk-p x u-mark u-mark-1)
                (mk-p x u-mark u-mark-1 ghost ghost launder
                      #:premade-defn premade-defn)
                (mk-p x u-mark u-mark-1 ghost ghost launder)
                (mk-p x u-mark u-mark-1 values ghost launder))))
    
    (define (plus-or p)
      (if or-macro?
          (vl-append gap-size
                     (ghost or-defn)
                     p)
          p))
    
    (define dots (code ....))
    ;; (define dots-mark (mark dots))
    
    (define let-rhs-mark (mark (code x)))
    (define let-body-mark (mark (code x)))
    
    (define green-let
      (let-syntax ([let (make-code-transformer #'(green let))]
                   [if (make-code-transformer #'(green if))]
                   [x (make-code-transformer #'(green x))])
        (code (let ([x #,let-rhs-mark])
                (if x x (#,let-body-mark))))))
    (define green-let-mark (locate (locate (mark green-let) green-let let-rhs-mark)
                                   green-let
                                   let-body-mark))
    (define green-let-mark-mark-1 (mark green-let-mark))
    (define green-let-mark-mark-2 (locate (locate (mark green-let-mark) green-let let-rhs-mark)
                                          green-let
                                          let-body-mark))
    
    (define introduced-x (encolors (if paint? (code x) (intro2 x)) module-scope intro1-scope))
    (define introduced-x-mark (mark introduced-x))

    (define original-y (code x))
    (define original-y-mark (mark original-y))
    
    (define result-or-id (encolors (if paint? (code or) (intro2 or)) module-scope intro1-scope))
    (define result-or (code (#,result-or-id #,introduced-x-mark (#,original-y-mark))))
    (define result-or-mark (locate (locate (locate (mark result-or) result-or original-y-mark)
                                           result-or introduced-x-mark)
                                   result-or result-or-id))
    
    (define (premade-or-expand #:body [dots-mark dots]
                               #:under-pattern-mark [under-pattern-mark #f]
                               #:post-or [post-or or-x-mark-mark-2]
                               #:comment [comment-mark (mark comment-mark)]
                               #:show-or-expand? [show-or-expand? #f]
                               #:green-let-mark [green-let-mark green-let-mark]
                               #:show-or? [show-or? show-or-expand?]
                               #:show-renamed? [show-renamed? show-or?]
                               #:show-nested? [show-nested? #f]
                               #:show-short-premade? [show-short-premade? (or show-renamed? show-nested?)]
                               #:show-done? [show-done? (or show-renamed? show-nested?)]
                               #:show-final? [show-final? show-done?]
                               #:show-match? [show-match? #f]
                               #:show-expand? [show-expand? show-final?])
      (let* ([ord (encolors (code premade-or) module-scope scope1)]
             [x (encolors (if show-renamed? (code x) (code x)) module-scope scope1)]
             [x.1 (cond
                   [show-renamed? original-y-mark]
                   [else (launder x)])]
             [x.2 macro-use-arg]
             [x2 introduced-x-mark])
        (let ([e (mk-p x
                       (let ([ex (if show-final? values ghost)])
                         (define p (if show-or-expand?
                                       (pin-over
                                        (pin-over green-let-mark
                                                  let-rhs-mark lt-find
                                                  x2)
                                        let-body-mark lt-find
                                        x.1)
                                       (cond
                                        [(not show-final?)
                                         (code (#,(ghost result-or-id) #,(ghost (code x)) (#,x.1)))]
                                        [(not show-renamed?)
                                         (code (#,result-or-id #,introduced-x-mark (#,x.1)))]
                                        [else
                                         result-or-mark])))
                         (if show-expand?
                             p
                             (cbl-superimpose (launder (ghost p)) dots-mark)))
                       under-pattern-mark 
                       #:post-or post-or
                       #:premade-defn (if show-short-premade?
                                          premade-defn
                                          long-premade-defn))])
          (let* ([p (if show-done?
                        e
                        (refocus (code #,e #,comment-mark)
                                 e))]
                 [p (if show-match?
                        (pin-bind-line p x.2 bind-e)
                        p)]
                 [p (if (and show-expand? (not show-final?))
                        (pin-bind-line p use-e x.1 #:rev? #t)
                        p)]
                 [p (if (and show-final? (not show-done?))
                        (pin-bind-line (pin-bind-line p tmpl-or result-or-id #:rev? #t)
                                       tmpl-x x2 #:rev? #t)
                        p)]
                 [p (if show-nested?
                        (refocus (code #,premade-defn
                                       code:blank
                                       (define (f x)
                                         #,(inset (real-encolor module-scope p) 0 (- gap-size) 0 0)
                                         ....)
                                       code:blank
                                       (define y ....))
                                 p)
                        p)]
                 [p (if or-macro?
                        (vl-append gap-size
                                   ((if show-or? values ghost)
                                    (refocus (cb-superimpose or-defn
                                                             green-let-mark-mark-1)
                                             or-defn))
                                   p)
                        p)])
            p))))
    
    (define-values (pre-macro-use-arg
                    pre-app-x pre-app-x-mark-mark-1
                    pre-macro-use pre-macro-use-mark pre-use-under-pattern-mark pre-use-body
                    pre-use-body0 pre-use-body0x pre-use-body1)
      (mk-body module-scope))
    (define-values (macro-use-arg
                    app-x app-x-mark-mark-1
                    macro-use macro-use-mark use-under-pattern-mark use-body
                    use-body0 use-body0x use-body1)
      (if paint?
          (mk-body module-scope scope1)
          (values pre-macro-use-arg
                  pre-app-x pre-app-x-mark-mark-1
                  pre-macro-use pre-macro-use-mark pre-use-under-pattern-mark pre-use-body
                  pre-use-body0 pre-use-body0x pre-use-body1)))

    (define (make-comment macro-use-mark)
      (hbl-append (colorize-comment (tt ";"))
                  (colorize-comment (t " was "))
                  macro-use-mark))
    (define comment
      (make-comment macro-use-mark))
    (define comment-mark
      (locate (mark comment) comment macro-use-mark))
    
    (define macro-use-mark-mark-1 (mark macro-use-mark))
    (define macro-use-mark-mark-2 (mark macro-use-mark))
    
    (define pre-app-x-mark (mark pre-app-x))
    (define app-x-mark (mark app-x))
    (define app-x-mark-mark-2 (mark app-x))

    (define or-x-mark-mark-1 (ghost (code (or x #,(mark app-x-mark)))))
    (define or-x-mark-mark-3 (ghost (code (or x #,(mark app-x-mark)))))
    (define or-x-mark-mark-2 (let ([p (ghost (code (or x #,app-x-mark-mark-2)))])
                               (refocus (vl-append 2 or-x-mark-mark-1 p)
                                        p)))
    (define or-x-mark-mark-2* (let ([p (ghost (code (or x #,app-x-mark)))])
                                (refocus (vl-append 2 or-x-mark-mark-1 p)
                                         p)))
    (define or-x-mark (ghost (code (or x #,app-x-mark-mark-2))))

    (define or-x (code (#,result-or-id #,introduced-x #,(mark app-x-mark))))

    (define (init-pre-app-x p)
      (with-handlers ([exn:fail? (lambda (x) p)])
        (pin-over p
                  pre-app-x-mark-mark-1 lt-find
                  pre-app-x-mark)))
    (define (init-app-x p)
      (with-handlers ([exn:fail? (lambda (x) p)])
        (pin-over p
                  app-x-mark-mark-1 lt-find
                  app-x-mark)))
    (define (post-app-x p)
      (with-handlers ([exn:fail? (lambda (x) p)])
        (pin-over p
                  app-x-mark-mark-2 lt-find
                  app-x-mark)))
    
    (define steps
     (append
      (if paint?
          null
          (list (plus-or (init-app-x pre-use-body0))
                (plus-or (init-app-x pre-use-body0x))
                (plus-or (init-app-x pre-use-body1))))
      (list (plus-or (init-pre-app-x pre-use-body)))
      (if paint?
          (list (plus-or (init-app-x use-body)))
          null)
      (list
       (lambda (n)
         (slide-pict
          (premade-or-expand #:body (fade-pict n
                                               macro-use-mark-mark-1 dots
                                               #:combine cbl-superimpose)
                             #:under-pattern-mark use-under-pattern-mark
                             #:comment (ghost (cellophane (make-comment macro-use-mark-mark-2) n)))
          (init-app-x macro-use-mark)
          macro-use-mark-mark-1
          use-under-pattern-mark
          n)))
      (list (cons 'slide (init-app-x (premade-or-expand #:under-pattern-mark macro-use-mark))))
      (list
       (lambda (n)
         (slide-pict
          (premade-or-expand #:under-pattern-mark macro-use-mark)
          app-x-mark
          app-x-mark-mark-1
          app-x-mark-mark-2
          n)))
      (list (premade-or-expand #:post-or or-x-mark-mark-2* #:under-pattern-mark macro-use-mark))
      (list
       (lambda (n)
         (slide-pict
          (premade-or-expand #:post-or or-x-mark-mark-2* #:under-pattern-mark macro-use-mark)
          or-x-mark
          or-x-mark-mark-1
          or-x-mark-mark-2*
          n)))
      (list (post-app-x (premade-or-expand #:post-or or-x-mark #:under-pattern-mark macro-use-mark)))
      (list
       (lambda (n)
         (post-app-x
          (slide-pict
           (premade-or-expand #:body or-x-mark-mark-3
                              #:post-or or-x-mark-mark-2
                              #:under-pattern-mark macro-use-mark)
           or-x-mark
           or-x-mark-mark-2
           or-x-mark-mark-3
           n))))
      (list (premade-or-expand #:show-done? #t))
      (if paint?
          null
          (list (premade-or-expand #:show-nested? #t)
                (premade-or-expand #:show-done? #t #:show-short-premade? #t)
                ;; (premade-or-expand #:show-renamed? #t)
                (premade-or-expand #:show-or? #t)
                (lambda (n)
                  (slide-pict
                   (premade-or-expand #:show-or-expand? #t
                                      #:green-let-mark green-let-mark-mark-2)
                   green-let-mark
                   green-let-mark-mark-1
                   green-let-mark-mark-2
                   n))
                (cons 'slide (premade-or-expand #:show-or-expand? #t))
                (cons (cons 'tilt green-let) (premade-or-expand #:show-or-expand? #t))))))
    
    (script
     (for/list ([step (in-list steps)])
       (if (pair? step) (cdr step) step))
     (for/list ([step (in-list steps)])
       (if (pair? step) (car step) 'immediate))
     (append
      (list (actor macro-use macro-use-mark)
            (actor app-x app-x-mark)
            (actor pre-app-x pre-app-x-mark)
            (actor or-x or-x-mark)
            (actor comment comment-mark)
            ;; (actor dots dots-mark)
            (actor green-let green-let-mark)
            (actor introduced-x introduced-x-mark)
            (actor original-y original-y-mark)
            (actor result-or result-or-mark))
      (if pair?
          (list (actor pre-macro-use pre-macro-use-mark))
          null)))))

(define (premade-or-example paint?)
  (script->picts (premade-or-example-script paint?)))

(define (just-or-example)
  (define inst-color "forestgreen")
  (define or-pattern (code (or a b)))
  (define or-match (ghost (launder or-pattern)))
  (define or-pattern+match (refocus (vl-append 2
                                               or-pattern
                                               (inset or-match (- (pict-width (tt "("))) 0 0 0))
                                    or-pattern))
  (define (make-or-template a b
                            #:keyword-color [keyword-color (current-keyword-color)]
                            #:id-color [id-color (current-id-color)])
    (parameterize ([current-keyword-color keyword-color]
                   [current-id-color id-color])
      (code (let ([x #,a])
              (if x x #,b)))))
  (define or-template
    (make-or-template (code a) (code b)))
  (define or-template-copy (ghost (launder or-template)))
  (define or-template+copy (refocus (vl-append or-template
                                               or-template-copy)
                                    or-template))
  (define or-initial-instance
    (make-or-template (ghost (code a)) (ghost (code b))
                      #:keyword-color inst-color
                      #:id-color inst-color))
  (define or-macro
    (code (define-syntax-rule #,or-pattern+match
            #,or-template+copy)))
  (define app-y (code (f)))
  (define app-x (code (x)))
  (define (make-or-use a b)
    (code (or #,a #,b)))
  (define or-use
    (make-or-use app-y app-x))
  (define (make-let-or-use or-use)
    (code (let ([x (λ () ....)])
            #,or-use)))
  (define let-or-use
    (make-let-or-use or-use))
  
  (define (d+u x y)
    (vl-append (pict-height or-macro)
               x
               y))
  
  (define or-match-find
    (shift lt-find 0 (pict-height or-pattern)))
  (define g-use (ghost or-use))
  
  (define (make-or-expand1 n)
    (slide-pict (d+u or-macro
                     (make-let-or-use g-use))
                or-use g-use or-match
                n))
  (define or-expanding1 (make-or-expand1 1))
  
  (define (make-or-expand2 n)
    (slide-pict or-expanding1
                or-initial-instance
                or-template
                or-template-copy
                n))
  
  (define g-app-x (ghost (launder app-x)))
  (define g-app-y (ghost (launder app-y)))
  (define g2-app-x (launder g-app-x))
  (define g2-app-y (launder g-app-y))
  
  (define green-template
    (make-or-template g2-app-y
                      g2-app-x
                      #:keyword-color inst-color
                      #:id-color inst-color))
  (define green-template-mark
    (ghost green-template))
  
  (define (make-or-expand3 n n2 #:template-show [template-show values])
    (let* ([template-inst 
            (make-or-template (fade-pict n
                                         (ghost (code a))
                                         g2-app-y)
                              (fade-pict n
                                         (ghost (code b))
                                         g2-app-x)
                              #:keyword-color inst-color
                              #:id-color inst-color)]
           [g-template-inst (ghost (launder template-inst))]
           [p (pin-over (d+u or-macro
                             (make-let-or-use
                              (fade-pict n2
                                         g-use
                                         g-template-inst)))
                        or-match lt-find
                        (cellophane (make-or-use g-app-y g-app-x)
                                    (- 1 n2)))]
           [p (slide-pict
               p
               (template-show template-inst)
               or-template-copy
               g-template-inst
               n2)]
           [p (slide-pict p app-x g-app-x g2-app-x n)]
           [p (slide-pict p app-y g-app-y g2-app-y n)])
      p))

  (define steps
    (list
     (d+u or-macro
          (ghost let-or-use))
     (d+u or-macro
          let-or-use)
     make-or-expand1
     or-expanding1
     make-or-expand2
     (make-or-expand2 1)
     (lambda (n) (make-or-expand3 n 0))
     (make-or-expand3 1 0)
     (lambda (n2) (make-or-expand3 1 n2))
     (make-or-expand3 1 1)
     (cons (cons 'tilt green-template)
           (make-or-expand3 1 1 #:template-show (lambda (p) green-template-mark)))
     ))
  (script->picts
   (script
    (for/list ([step (in-list steps)])
      (if (pair? step) (cdr step) step))
    (for/list ([step (in-list steps)])
      (if (pair? step) (car step) 'immediate))
    (list
     (actor green-template green-template-mark)))))

(define (scopes-example #:more-pedantic? [more-pedantic? #f])
  (let* ([x1 (code x)]
         [y1 (code x)]
         [x2 (launder x1)]
         [y2 (launder y1)]
         [x3 (code y)]
         [y3 (launder y1)]
         [x4 (launder x3)]
         [y4 (launder y1)]
         [y5 (launder y1)])
    (define (mk-p body post encolors
                  #:encolor [encolor encolor])
      (pad-use-of-or
       (code
        (define #,(encolors x1 scope1) 1)
        #,(encolor
           scope1
           #:out 3
           (code
            code:blank
            #,(let-syntax ([let (make-code-transformer #'(encolors (code let) scope1 #:int? #t))])
                (code
                 (let ([#,(encolors y1 scope1 scope2) #,(encolors x2 scope1 #:int? #t)])
                   #,(encolor
                      scope2
                      #:out 2
                      (let-syntax ([λ (make-code-transformer #'(encolors (code λ) scope1 scope2 #:int? #t))])
                        (code
                         (λ (#,(encolors x3 scope1 scope2 scope3))
                           #,(encolor scope3 #:out 1 body))))))))))
        #,post)))
    (define (mk-p1 encolors #:encolor [encolor encolor])
      (define or-id (encolors (code or) scope1 scope2 scope3))
      (mk-p (code (#,or-id #,(encolors x4 scope1 scope2 scope3 #:int? #t) #,(encolors y2  scope1 scope2 scope3 #:int? #t)))
            two-blank
            encolors
            #:encolor encolor))
    (define encolors-last (lambda (p #:int? [int? #f] . cols) (encolor #:fill? #t (last cols) p)))

    (define (mk-expanded encolor* encolors* [encolors** encolors]
                         #:encolors [encolors encolors]
                         #:encolor [encolor encolor])
      (mk-p (let-syntax ([let (make-code-transformer #'(encolors** (code let) scope4))])
              (code (let ([#,(encolors* y3 scope4 scope5) #,(encolors x4 scope1 scope2 scope3)])
                      #,(encolor*
                         scope5
                         (let* ([if (encolors* (code if) scope4 scope5)]
                                [y4 (encolors* y4 scope4 scope5)]
                                [y5 (encolors* y5 scope4 scope5)]
                                [y2 (encolors* y2  scope1 scope2 scope3 scope5)])
                           (code
                            (#,if #,y4 #,y5 #,y2)))))))
            one-blank
            encolors
            #:encolor encolor))
    
    (define (extract-layers mk layers)
      (append
       (let ([layers layers])
         (for/list ([layer (in-list layers)])
           (define saved null)
           (define (save p)
             (set! saved (cons p saved))
             p)
           (define all-p
             (mk (lambda (p #:int? [int? #f] . cols)
                   (if (and (not int?) (equal? (last cols) layer))
                       (save (encolor #:fill? #t layer (ghost p)))
                       p))
                 #:encolor (lambda (c p #:out [out 0])
                             (if (equal? c layer)
                                 (save (encolor c #:out out (ghost p) #:fill? #t))
                                 p))))
           (for/fold ([p (ghost all-p)]) ([s (in-list saved)])
             (pin-over p s lt-find s))))
       (list (mk (lambda (p #:int? [int? #t] . cols) p) #:encolor (lambda (c p #:out [out 0]) p)))))
    
    (define (add-note p alongside note)
      (define-values (x y) (lt-find p alongside))
      (pin-over p
                (- (pict-width p) (pict-width note) (* gap-size)) y
                note))
    (define (is-subset a b)
      (hbl-append a (t " ⊆ ") b))
    (define (is-not-subset a b)
      (hbl-append a (t " ⊈ ") b))
    
    (define (bright p e)
      (define s 5)
      (define-values (x y) (lt-find p e))
      (pin-under p
                 (- x s) (- y s)
                 (encolor arrow-color (launder (inset e s)))))
    
    (define (no-encolor col p #:out [out 0] #:fill? [fill? #f] #:skip [skip 0])
      p)
    
    (define x-ref (launder (encolors (ghost y2) scope1 scope2 scope3)))
    
    (define simple-colors (mk-p1 encolors #:encolor no-encolor))
    (define simple-colors+x-bind (pin-bind-line simple-colors
                                                y2 y1))
    (define simple-colors+ref (add-note simple-colors+x-bind
                                        y2
                                        x-ref))
    (define cand2 (launder (encolors (ghost y1) scope1 scope2)))
    (define simple-colors+cand2 (add-note simple-colors+ref
                                          y1
                                          (is-subset cand2 x-ref)))
    (define cand1 (launder (encolors (ghost x1) scope1)))
    (define simple-colors+cand1 (add-note simple-colors+cand2
                                          x1
                                          (is-subset cand1 x-ref)))
    (define simple-colors+decide (let-values ([(x1 y1) (lb-find simple-colors+cand1 cand1)]
                                              [(x2 y2) (lt-find simple-colors+cand1 cand2)])
                                   (define sub (rotate (t "⊆") (/ pi -2)))
                                   (pin-over simple-colors+cand1
                                             (+ x1 (/ (- (pict-width cand1) (pict-width sub)) 2))
                                             (/ (+ y1 y2 (- (pict-height sub))) 2)
                                             sub)))
    
    (define normal-red
      (mk-expanded (lambda (c p) p) (lambda (p #:int? [int? #f] . cs) 
                                 (apply encolors p (reverse (cdr (reverse cs)))))
                   #:encolor no-encolor))
    (define post-red
      (mk-expanded no-encolor encolors #:encolor no-encolor))
    
    (define post-x-ref (launder (encolors (ghost y2) scope1 scope2 scope3 scope5)))
    
    (define (add-paras p paras)
      (refocus (vc-append gap-size
                          p
                          (inset paras (- gap-size) 0 0 0))
               p))
    
    (append
     (list (extract-layers mk-p1 `(,scope1 ,scope2 ,scope3))
           (mk-p1 encolors)
           simple-colors
           simple-colors+x-bind
           (bright simple-colors+ref y2)
           (bright simple-colors+cand2 y1)
           (add-paras (bright simple-colors+cand2 y1)
                      (is-subset (t "binding scopes ") (t " reference scopes")))
           (bright simple-colors+cand1 x1)
           (bright (bright simple-colors+decide y1) cand2)
           (add-paras (bright (bright simple-colors+decide y1) cand2)
                      (hbl-append (t "use candidate with ") (it "biggest") (t " subset")))
           normal-red
           (extract-layers (lambda (encs #:encolor enc)
                             (define (encs-x p #:int? [int? #f] . cs)
                               (apply encs p #:int? int? (remove scope5 cs)))
                             (mk-expanded enc
                                          encs-x
                                          encs-x
                                          #:encolors encs-x
                                          #:encolor enc))
                           `(,scope1 ,scope2 ,scope3 ,scope4))
           normal-red
           (bright (add-note normal-red y3 (is-not-subset (encolors (ghost y3) scope4) x-ref))
                   y2)
           (add-paras (mk-expanded encolor encolors #:encolor no-encolor)
                      (vl-append
                       (/ gap-size 2)
                       (t "Expander introduces a fresh scope when it")
                       (hbl-append (t " ∙ expands a macro (e.g., ") (encolors (tt " ") scope4) (inset (t ")") 4 0 0 0))
                       (hbl-append (t " ∙ finds a binding form (e.g., ")
                                   (encolors (tt " ") scope1) (t ", ")
                                   (encolors (tt " ") scope2) (t ", ")
                                   (encolors (tt " ") scope3) (t ", ")
                                   (encolors (tt " ") scope5)
                                   (inset (t ")") 4 0 0 0)))))
     (if more-pedantic?
         (list (bright
                (add-note (add-note post-red y3 (is-not-subset (encolors (ghost y3) scope4 scope5) post-x-ref))
                          y1 (is-subset cand2 post-x-ref))
                y2)
               (let ([encolors/add-before-red (lambda (p #:int? [int? #f] . cs)
                                                (if (and (pair? cs)
                                                         (equal? scope4 (car cs)))
                                                    (apply encolors p scope6 cs)
                                                    (apply encolors p cs)))])
                 (let ([p (mk-expanded no-encolor encolors/add-before-red encolors/add-before-red
                                       #:encolor no-encolor)])
                   (vl-append
                    gap-size
                    p
                    (hbl-append (encolors (ghost (tt "x")) scope6)
                                (t " = scope of definition of ")
                                (code or))))))
         null))))
