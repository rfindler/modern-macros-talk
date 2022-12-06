#lang racket/base
(require racket/set
         pict
         "script.rkt"
         "card.rkt")

(provide script->planneds
         (struct-out planned))

(struct planned (cards+positions width height))

(define (script->planneds s #:adjust-card [adjust-card values])
  (let loop ([prev-scene #f]
             [scenes (script-scenes s)]
             [transitions (script-transitions s)])
    (cond
     [(null? scenes) null]
     [(eq? 'immediate (car transitions))
      ;; No further movement:
      (define p (script-scene->pict s (car scenes)))
      (define ocs (pict->offset-cards p))
      (cons
       (planned
        (for/list ([c (in-list ocs)])
          (cons (adjust-card (vector-ref c 0))
                (lambda (t)
                  (values (vector-ref c 1)
                          (vector-ref c 2)))))
        (pict-width p)
        (pict-height p))
       (loop (car scenes) (cdr scenes) (cdr transitions)))]
     [else
      ;; Must be a 'slide transition
      (cons
       (planned
        (append
         (plan-sliding-actors s prev-scene (car scenes) adjust-card)
         (plan-sliding-scene (pict->offset-cards prev-scene)
                             (pict->offset-cards (car scenes))
                             adjust-card))
        (pict-width prev-scene)
        (pict-height prev-scene))
       (loop (car scenes) (cdr scenes) (cdr transitions)))])))
        
(define (plan-sliding-scene start-ocs end-ocs adjust-card)
  (define oc1 (list->set start-ocs))
  (define oc2 (list->set end-ocs))
  
  (define common (set-intersect oc1 oc2))
  (define unmatched-start (set-subtract oc1 common))
  (define unmatched-end (set-subtract oc2 common))
  
  (unless (= (set-count unmatched-start)
             (set-count unmatched-end))
    (error 'plan-movement "count mismatch"))
  
  (define (sort-by-dist s)
    (sort (set->list s)
          <
          #:key (lambda (v)
                  (+ (expt (vector-ref v 1) 2)
                     (expt (vector-ref v 2) 2)))))
  
  (append
   ;; Common cards don't move (and keep them in order):
   (for/list ([c (in-list start-ocs)]
              #:when (set-member? common c))
     (cons (adjust-card (vector-ref c 0))
           (lambda (t)
             (values (vector-ref c 1)
                     (vector-ref c 2)))))
   ;; Match up remaining and compute movement:
   (for/list ([oc1 (sort-by-dist unmatched-start)]
              [oc2 (sort-by-dist unmatched-end)])
     (unless (eq? (vector-ref oc1 0)
                  (vector-ref oc2 0))
       (error 'plan-movement "sort mismatch"))
     (cons (adjust-card (vector-ref oc1 0))
           (lambda (n)
             (values
              (+ (* (- 1 n) (vector-ref oc1 1))
                 (* n (vector-ref oc2 1)))
              (+ (* (- 1 n) (vector-ref oc1 2))
                 (* n (vector-ref oc2 2)))))))))

(define (plan-sliding-actors s prev-scene scene adjust-card)
  (apply
   append
   (for/list ([a (in-list (script-actors s))]
              #:when (or (scene-actor-position prev-scene a)
                         (scene-actor-position scene a)))
     (define from (or (scene-actor-position prev-scene a)
                      (cons 2000 2000)))
     (define to (or (scene-actor-position scene a)
                    (cons -2000 2000)))
     (define dx (- (car to) (car from)))
     (define dy (- (cdr to) (cdr from)))
     (define ocs (pict->offset-cards (actor-pict a)))
     (for/list ([oc (in-list ocs)])
       (cons (adjust-card (vector-ref oc 0))
             (lambda (n)
               (values (+ (vector-ref oc 1) (car from) (* n dx))
                       (+ (vector-ref oc 2) (cdr from) (* n dy)))))))))
  
