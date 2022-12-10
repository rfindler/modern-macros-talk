#lang racket
(require "../lib/setup.rkt"
         "../lib/util.rkt"
         slideshow
         slideshow/play
         slideshow/code)

(provide open-compiler)

(define (open-compiler)
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
  (define front-end-small (cc-superimpose the-box front-end))
  (define parser-box (cc-superimpose the-box parser))
  (define middle-end-box (add-self-loop (cc-superimpose the-box middle-end)))
  (define back-end-box (cc-superimpose the-box back-end))

  (define clauses
    (list (code [(if a b c)       (if (compile a)
                                      (compile b)
                                      (compile c))])
          (code [(+ a ...)        (+ (compile a) ...)])
          (code [(and a b)        (compile (if a b #f))])
          (code [(or a b)         (compile (let ([x a])
                                             (if x x b)))])
          (code [(let ([x a]) b)  (let ([x (compile a)])
                                    (compile b))])))

  (define (slide-and-scale/all main from n)
    (for/fold ([main main])
              ([clause (in-list clauses)])
      (slide-and-scale main from clause n)))

  (define (slide-and-scale main from clause n)
    (define padding 12)
    (define-values (fx fy) (lt-find main from))
    (define-values (cx cy) (lt-find main clause))
    (define-values (cpx cpy) (values (- cx (/ padding 2)) (- cy (/ padding 2))))
    (pin-over
     main
     (interpolate fx cpx n)
     (interpolate fy cpy n)
     (linewidth box-linewidth
                (frame
                 (blank (interpolate (pict-width from) (+ (pict-width clause) padding) n)
                        (interpolate (pict-height from) (+ (pict-height clause) padding) n))))))
  
  (define (compiler/code n)
    (inset
     (code
      (define (compile exp)
        (syntax-parse exp
          #,(apply vl-append
                   (for/list ([clause (in-list clauses)])
                     (define extra-space 14)
                     (inset clause
                            0 (* n extra-space)
                            0 (* n extra-space)))))))
     20))

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
  
  (play-n
   (λ (n1 n2 n3)
     (define big-box
       (box-size-interpolate the-box (compiler/code 0) n1))
     (define main
       (slide-and-scale/all
        (add-arrows-between-boxes
         n3
         parser-box
         (refocus (cc-superimpose (cc-superimpose
                                   (cellophane front-end (- 1 n2))
                                   big-box)
                                  (cellophane (compiler/code n3) n2))
                  big-box)
         middle-end-box
         back-end-box) 
        big-box
        n3))
     (slide-center-between
      main
      main
      big-box n1))))

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
  (open-compiler))
