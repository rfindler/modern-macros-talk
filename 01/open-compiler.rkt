#lang racket
(require "../lib/setup.rkt"
         "../lib/util.rkt"
         slideshow
         slideshow/play
         slideshow/code)

(provide open-compiler-part1 open-compiler-part2)

(define part-2-phases 2)

(define (open-compiler-part1)
  (define play-proc (mk-open-compiler-proc #f))
  (define total-number-of-phases (procedure-arity play-proc))
  (define part-1-phases (- total-number-of-phases part-2-phases))
  (play-n
   (procedure-reduce-arity
    (λ args
      (apply play-proc (append args (make-list (- total-number-of-phases part-1-phases) 0))))
    part-1-phases)))

(define (open-compiler-part2)
  (define play-proc (mk-open-compiler-proc #t))
  (define total-number-of-phases (procedure-arity play-proc))
  (define part-1-phases (- total-number-of-phases part-2-phases))
  (play-n
   (procedure-reduce-arity
    (λ args
      (apply play-proc (append (make-list part-1-phases 1) args)))
    part-2-phases)))

(define (mk-open-compiler-proc kill-issues?)
  (define parser (scale/improve-new-text (t "parser") 1.5))
  (define front-end (scale/improve-new-text (vc-append (t "front")
                                                       (t "end")) 1.5))
  (define middle-end (scale/improve-new-text (vc-append (t "middle")
                                                        (t "end")) 1.5))
  (define back-end (scale/improve-new-text (vc-append (t "back")
                                                      (t "end")) 1.5))
  (define s (+ (max (pict-width front-end)
                    (pict-width parser)
                    (pict-width middle-end)
                    (pict-width back-end))
               100))
  (define the-box (linewidth box-linewidth (frame (blank s s))))
  (define parser-box (cc-superimpose the-box parser))
  (define middle-end-box (add-self-loop (cc-superimpose the-box middle-end)))
  (define back-end-box (cc-superimpose the-box back-end))

  (define exp (code exp))
  
  (λ (n-a1 n-a2 n-a3 n-a4 n-b3 n-b4 n-b5 n-b6 n-highlight-exp n-fade-except-or)
    (define Γ (cellophane (code Γ) (if (= 1 n-b6) 1 0)))
    (define maybe-Γ
      (clip/to-zero
       (inset Γ 0 0 (* (- (+ (pict-width (tt " ")) (pict-width Γ))) (- 1 n-b6)) 0)))
    (define plain-clauses
      (list (code [(if a b c) (if (compile a #,maybe-Γ)
                                  (compile b #,maybe-Γ)
                                  (compile c #,maybe-Γ))])
            (code [(+ a ...) (+ (compile a #,maybe-Γ) ...)])
            (code [(and a b) (compile (if a b #f) #,maybe-Γ)])
            (code [(or a b) (compile (let ([x a])
                                       (if x x b)) #,maybe-Γ)])
            (code [(let ([x a]) b) (let ([x (compile a #,maybe-Γ)])
                                     (compile b #,maybe-Γ))])))

    (define or-clause-index 3)
     
    (define fading-for-non-or-clauses (interpolate 1 .1 n-fade-except-or))
    (define clauses
      (for/list ([clause (in-list plain-clauses)]
                 [i (in-naturals)])
        (cond
          [(= i or-clause-index) clause]
          [else (cellophane clause fading-for-non-or-clauses)])))

    (define (slide-and-scale/all main from n)
      (for/fold ([main main])
                ([clause (in-list clauses)]
                 [i (in-naturals)])
        (slide-and-scale main from clause n (not (= i or-clause-index)))))

    (define (slide-and-scale main from clause n fade-it?)
      (define padding 12)
      (define-values (fx fy) (lt-find main from))
      (define-values (cx cy) (lt-find main clause))
      (define-values (cpx cpy) (values (- cx (/ padding 2)) (- cy (/ padding 2))))
      (define to-frame
        (blank (interpolate (pict-width from) (+ (pict-width clause) padding) n)
               (interpolate (pict-height from) (+ (pict-height clause) padding) n)))
      (pin-over
       main
       (interpolate fx cpx n)
       (interpolate fy cpy n)
       (linewidth box-linewidth
                  (if fade-it?
                      (frame+cellophane to-frame fading-for-non-or-clauses)
                      (frame to-frame)))))
  
    (define things-to-do
      (if kill-issues?
          (blank)
          (vl-append
           (t "Issues:")
           (hc-append
            (blank 20 0)
            (vl-append
             10
             (t "• identify the next case")
             (t "• find the transformation")
             (t "• open recursion")
             (vl-append (t "• facilitating communication")
                        (hbl-append (ghost (t "• ")) (t "between cases"))))
            (blank 20 0))
           (blank 0 20)
           (vl-append (t "The overall transformation")
                      (t "is implicit in the cases"))
           (blank 0 100))))

    (define (compiler/code n)
      (hc-append (inset
                  (code
                   (define (compile #,exp #,maybe-Γ)
                     (syntax-parse exp
                       #,(apply vl-append
                                (for/list ([clause (in-list clauses)])
                                  (define extra-space 14)
                                  (inset clause
                                         0 (* n extra-space)
                                         0 (* n extra-space)))))))
                  20 20 40 20) ;; why we need an extra 20 to the right I have no idea
                 (inset (clip (inset (cellophane things-to-do (if (= n-b5 1) 1 0))
                                     0 0 (- (* (pict-width things-to-do) (- 1 n-b5))) 0))
                        -20 0 0 0)))

    (define (add-arrows-between-boxes n . boxes)
      (define main (apply hc-append 80 boxes))
      (for/fold ([main main])
                ([box1 (in-list boxes)]
                 [box2 (in-list (cdr boxes))])
        (pin-arrow-line arrowhead-size main
                        box1 rc-find
                        box2 lc-find
                        #:alpha (- 1 n)
                        #:line-width box-linewidth)))
  
    (define big-box
      (box-size-interpolate the-box (compiler/code 0) n-a1))
    (define main
      (slide-and-scale/all
       (add-arrows-between-boxes
        n-a3
        (cellophane parser-box (- 1 n-b5))
        (hc-append (refocus (cc-superimpose (cc-superimpose
                                             (cellophane front-end (- 1 n-a2))
                                             big-box)
                                            (cellophane (compiler/code n-a3) n-a2))
                            big-box))
        (cellophane middle-end-box (- 1 n-b5))
        (cellophane back-end-box (- 1 n-b5)))
       big-box
       n-a3))
    (define slid-over
      (slide-center-between
       main
       main
       big-box n-a1))
    (add-line-from-if-case-to-let-case
     (add-line-from-if-case-to-and-case
      (add-line-from-and-case-to-if-case
       (add-line-to-if-case
        (show-circle
         (show-circle slid-over exp n-highlight-exp 0)
         exp n-a4 n-b5)
        exp (car clauses) n-a4 n-b5)
       (car clauses) (list-ref clauses 2) n-b4 n-b5)
      (car clauses) (list-ref clauses 2) n-b3 (fast-start n-b4) n-b5)
     (car clauses) (list-ref clauses 4) n-b4 n-b5)))

(define (clip/to-zero p)
  (cond
    [(<= (pict-width p) 0) p]
    [else (clip p)]))

(define (frame+cellophane p n)
  (cc-superimpose
   (cellophane (frame (ghost (launder p))) n)
   p))
  
(define (show-circle main exp n go-away-n)
  (define ghosted (ghost (launder exp)))
  (define circle-around-exp
    (refocus (cc-superimpose
              ghosted
              (cellophane
               (linewidth
                box-linewidth
                (colorize
                 (rounded-rectangle (+ (pict-width exp) 10)
                                    (+ (pict-height exp) 10))
                 "red"))
               (* n (- 1 go-away-n))))
              ghosted))
  (pin-over
   main
   exp
   lt-find
   circle-around-exp))

(define (add-line-to-if-case main exp if-clause n go-away-n)
  (cc-superimpose
   main
   (cellophane
    (linewidth
     box-linewidth
     (colorize
      (launder
       (pin-arrow-line
        30
        (ghost main)
        exp
        (dxdy-find (/ box-linewidth 2) 0 rc-find)
        if-clause
        (%%-find 2/3 0)
        #:start-angle 0
        #:end-angle (- (/ pi 2))
        ))
      "red"))
    (* n (- 1 go-away-n)))))

(define (add-line-from-if-case-to-and-case main if-clause and-clause n arrow-move-n go-away-n)
  (cc-superimpose
   main
   (cellophane
    (linewidth
     box-linewidth
     (colorize
      (launder
       (pin-arrow-line
        30
        (ghost main)
        if-clause
        (%%-find 0 (interpolate 1/2 2/3 arrow-move-n))
        and-clause
        lc-find
        #:start-angle pi
        #:start-pull 1/2
        #:end-angle 0
        #:end-pull 1/2
        ))
      "red"))
    (* n (- 1 go-away-n)))))

(define (add-line-from-and-case-to-if-case main and-clause if-clause n go-away-n)
  (cc-superimpose
   main
   (cellophane
    (linewidth
     box-linewidth
     (colorize
      (launder
       (pin-arrow-line
        30
        (ghost main)
        if-clause
        rc-find
        and-clause
        rc-find
        #:start-angle 0
        #:start-pull 1/2
        #:end-angle pi
        #:end-pull 1/2
        ))
      "red"))
    (* n (- 1 go-away-n)))))

(define (add-line-from-if-case-to-let-case main if-clause let-clause n go-away-n)
  (cc-superimpose
   main
   (cellophane
    (linewidth
     box-linewidth
     (colorize
      (launder
       (pin-arrow-line
        30
        (ghost main)
        if-clause
        (%%-find 0 1/3)
        let-clause
        lc-find
        #:start-angle pi
        #:start-pull 1/3
        #:end-angle 0
        #:end-pull 1/3
        ))
      "red"))
    (* n (- 1 go-away-n)))))

(define ((%%-find %w %h) p m)
  (define-values (l t) (lt-find p m))
  (define-values (r b) (rb-find p m))
  (values (+ l (* %w (- r l)))
          (+ t (* %h (- b t)))))

(define ((dxdy-find dx dy find) p m)
  (define-values (x y) (find p m))
  (values (+ x dx) (+ y dy)))

(define (add-self-loop p)
  (define dx (* (pict-width p) 1/8))
  (cc-superimpose
   p
   (linewidth
    box-linewidth
    (launder
     (pin-arrow-line
      arrowhead-size
      (ghost p)
      p
      (wrap-find ct-find dx 0)
      p
      (wrap-find ct-find (- dx) 0)
      #:start-pull 2.5
      #:end-pull 2
      #:start-angle (/ pi 3)
      #:end-angle (/ pi -3))))))


(define (wrap-find xx-find dx dy)
  (λ args
    (define-values (x y) (apply xx-find args))
    (values (+ x dx) (+ y dy))))

(define box-linewidth 8)
(define arrowhead-size (* box-linewidth 4))

;; this has the center of `c1` at the center of the entire slide when `n`
;; is 0 and the center of `c2` at the center of the entire slide when `n`
;; is 1; in between it interpolates between those
(define (slide-center-between main c1 c2 n)
  (define big-blank (blank client-w client-h))
  (define-values (c1x c1y) (cc-find main c1))
  (define-values (c2x c2y) (cc-find main c2))
  (define b (blank))
  (cc-superimpose
   big-blank
   (refocus (pin-over
             main
             (interpolate c1x c2x n)
             (interpolate c1y c2y n)
             b)
            b)))

(define (box-size-interpolate box1 box2 n)
  (blank (interpolate (pict-width box1) (pict-width box2) n)
         (interpolate (pict-height box1) (pict-height box2) n)))

(module+ main
  (open-compiler-part1)
  (slide (t "between"))
  (open-compiler-part2))
