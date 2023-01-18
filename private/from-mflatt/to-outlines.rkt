#lang racket/base
(require racket/draw
         racket/class
         racket/match
         racket/math)

(provide record-dc->outlines
         (struct-out outline))

(struct outline (color path))

(define (record-dc->outlines dc)
  (define font (make-font))
  (define text-color (make-color 0 0 0))
  (define pen-color (make-color 0 0 0))
  (define brush-color (make-color 255 255 255))
  (define pen-style 'solid)
  (define brush-style 'solid)
  (define pen-width 1.0)
  
  (define (add-points p points dx dy)
    (send p move-to (+ dx (caar points)) (+ dy (cdar points)))
    (let loop ([points (cdr points)])
      (cond
       [(null? points) (void)]
       [(pair? (car points))
        (define pt (car points))
        (send p line-to (+ dx (car pt)) (+ dy (cdr pt)))
        (loop (cdr points))]
       [else
        (define pt (car points))
        (define ptn (cadr points))
        (send p curve-to
              (+ dx (vector-ref pt 0)) (+ dy (vector-ref pt 1))
              (+ dx (vector-ref pt 2)) (+ dy (vector-ref pt 3))
              (+ dx (car ptn)) (+ dy (cdr ptn)))
        (loop (cddr points))])))
  
  (define (add-edge-points p points dx dy #:start? [start? #t])
    (define (get-delta prev-pt pt)
      (define vec (make-rectangular (- (car pt) (car prev-pt))
                                    (- (cdr pt) (cdr prev-pt))))
      (define to-edge (make-polar (/ pen-width 2.0)
                                  (- (angle vec) (/ pi 2))))
      (values (+ dx (real-part to-edge))
              (+ dy (imag-part to-edge))))
    (let loop ([prev-pt (car points)] [points (cdr points)] [start? start?])
      (cond
       [(null? points) (void)]
       [(pair? (car points))
        (define pt (car points))
        (cond
         [(equal? pt prev-pt)
          (loop pt (cdr points) start?)]
         [else
          (define-values (dx dy) (get-delta prev-pt pt))
          (if start?
              (send p move-to (+ dx (car prev-pt)) (+ dy (cdr prev-pt)))
              (send p line-to (+ dx (car prev-pt)) (+ dy (cdr prev-pt))))
          (send p line-to (+ dx (car pt)) (+ dy (cdr pt)))
          (loop pt (cdr points) #f)])]
       [else
        (define pt (car points))
        (define ptn (cadr points))
        (define-values (dx1 dy1) (get-delta prev-pt
                                            (cons (vector-ref pt 0) 
                                                  (vector-ref pt 1))))
        (define-values (dx2 dy2) (get-delta (cons (vector-ref pt 2) 
                                                  (vector-ref pt 3))
                                            ptn))
        (if start?
            (send p move-to (+ dx1 (car prev-pt)) (+ dy1 (cdr prev-pt)))
            (send p line-to (+ dx1 (car prev-pt)) (+ dy1 (cdr prev-pt))))
        (define ddx (/ (+ dx1 dx2) 2.0))
        (define ddy (/ (+ dy1 dy2) 2.0))
        (send p curve-to
              (+ ddx (vector-ref pt 0)) (+ ddy (vector-ref pt 1))
              (+ ddx (vector-ref pt 2)) (+ ddy (vector-ref pt 3))
              (+ dx2 (car ptn)) (+ dy2 (cdr ptn)))
        (loop ptn (cddr points) #f)])))
  
  (define (points->fill-outline closed-points [dx 0] [dy 0])
    (define p (new dc-path%))
    (for ([c (in-list closed-points)])
      (add-points p c dx dy)
      (send p close))
    (outline brush-color p))
  
  (define (points->stroke-outline closed-points open-points [dx 0] [dy 0])
    (define p (new dc-path%))
    (for ([c (in-list closed-points)])
      (add-edge-points p c dx dy)
      (send p close)
      (add-edge-points p (reverse-points c) dx dy)
      (send p close))
    (unless (null? open-points)
      (add-edge-points p open-points dx dy)
      (add-edge-points p (reverse-points open-points) dx dy #:start? #f)
      (send p close))
    (outline pen-color p))

  (define (reverse-points ps)
    (let loop ([ps ps] [a null])
      (cond
       [(null? ps) a]
       [(pair? (car ps)) (loop (cdr ps)
                               (cons (car ps) a))]
       [else
        (define v (car ps))
        (loop (cdr ps)
              (cons (vector (vector-ref v 2) (vector-ref v 3)
                            (vector-ref v 0) (vector-ref v 1))
                    a))])))
  
  (define (maybe-add pen-outline brush-outline outlines)
    (cond
     [(and pen-outline brush-outline)
      (list* pen-outline brush-outline outlines)]
     [pen-outline
      (cons pen-outline outlines)]
     [brush-outline
      (cons brush-outline outlines)]
     [else outlines]))

  (define (recorded-datum->outlines r)
    (for/fold ([outlines null]) ([c (in-list r)])
      (match c
        [`(set-font (,size ,face ,family ,style ,weight ,underline ,smoothing ,size-in-pixels))
         (set! font (make-font #:size size
                               #:face face
                               #:family family
                               #:style style
                               #:weight weight
                               #:underlined? underline
                               #:smoothing smoothing
                               #:size-in-pixels? size-in-pixels))
         outlines]
        [`(set-text-foreground (,r ,g ,b ,a))
         (set! text-color (make-color r g b a))
         outlines]
        [`(do-set-brush! ((,r ,g ,b ,a) ,style ,_ ,_ ,_))
         (set! brush-style style)
         (set! brush-color (make-color r g b a))
         outlines]
        [`(do-set-pen! ((,r ,g ,b ,a) ,width ,style ,_ ,_ ,_))
         (set! pen-style style)
         (set! pen-color (make-color r g b a))
         (set! pen-width width)
         outlines]
        [`(draw-text ,str ,x ,y ,combine? ,offset ,angle)
         (cond
          [(regexp-match? #px"^\\s*$" str)
           outlines]
          [else
           (define p (new dc-path%))
           (send p text-outline font str x y combine?)
           (cons (outline text-color p) outlines)])]
        [`(draw-path (,closed-points . ,open-points) ,dx ,dy ,mode)
         (maybe-add 
          (and (not (eq? 'transparent pen-style))
               (points->stroke-outline closed-points open-points dx dy))
          (and (not (eq? 'transparent brush-style))
               (points->fill-outline closed-points dx dy))
          outlines)]
        [`(draw-line ,x1 ,y1 ,x2 ,y2)
         (if (eq? 'transparent pen-style)
             outlines
             (cons (points->stroke-outline null (list (cons x1 y1) (cons x2 y2)))
                   outlines))]
        [`(draw-polygon ,points ,dx ,dy ,fill-style)
         (maybe-add
          (and (not (eq? 'transparent pen-style))
               (points->stroke-outline (list points) null dx dy))
          (and (not (eq? 'transparent brush-style))
               (points->fill-outline (list points) dx dy))
          outlines)]
        [`(draw-rectangle ,x1 ,y1 ,x2 ,y2)
         (define points (list (cons x1 y1) (cons x2 y1) (cons x2 y2) (cons x1 y2)))
         (maybe-add
          (and (not (eq? 'transparent pen-style))
               (points->stroke-outline (list points) null))
          (and (not (eq? 'transparent brush-style))
               (points->fill-outline (list points)))
          outlines)]
        [else
         ;; (log-error "~s" c)
         outlines])))

  (recorded-datum->outlines (send dc get-recorded-datum)))
