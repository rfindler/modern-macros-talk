#lang racket/base
(require pict
         racket/set
         racket/math)

(provide script->picts
         script-scene->pict
         scene-actor-position
         (struct-out script)
         (struct-out actor)
         (struct-out middle-play)
         tilt)

(struct script (scenes transitions actors))
(struct actor (pict mark))
(struct middle-play (p))

(define (scene-actor-position scene a)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (define-values (x y) (lt-find scene (actor-mark a)))
    (cons x y)))

(define (script-scene->pict s scene
                            #:only [only-picts #f]
                            #:except [except-picts #f])
  (for/fold ([scene scene]) ([a (in-list (script-actors s))]
                             #:when (or (not only-picts)
                                        (set-member? only-picts (actor-pict a)))
                             #:unless (and except-picts
                                           (set-member? except-picts (actor-pict a))))
    (define p (scene-actor-position scene a))
    (if p
        (pin-under scene (car p) (cdr p) (actor-pict a))
        scene)))

(define (tilt p dir t color #:rot-dir [rot-dir 1])
  (define dy (* dir t 40))
  (define dx (* dir t 60))
  (define (plane p)
    (if color
        (refocus (cc-superimpose (colorize
                                  (frame
                                   (cellophane
                                    (filled-rectangle (+ 32 (pict-width p))
                                                      (+ 32 (pict-height p)))
                                    0.15))
                                  color)
                                 p)
                 p)
        p))
  (inset (rotate (scale (rotate (plane p) (* pi rot-dir 1/4)) (+ 1 (* t 0.2)) (- 1 (* t 0.3)))
                 (* pi rot-dir -1/4))
         dx (- dy) (- dx) dy))

(define (script->picts s)
  (for/list ([scene (in-list (script-scenes s))]
             [transition (in-list (script-transitions s))])
    (cond
     [(and (pair? transition)
           (eq? 'tilt (car transition)))
      (define p1 (script-scene->pict s scene #:except (set (cdr transition))))
      (define p2 (script-scene->pict s (ghost scene) #:only (set (cdr transition))))
      (lambda (t)
        (refocus (cc-superimpose (ghost p1)
                                 (tilt (launder p1) +1 t "blue")
                                 (tilt (launder p2) -1 t "forestgreen"))
                 p1))]
     [(procedure? scene)
      (middle-play
       (lambda (t)
         (script-scene->pict s (scene t))))]
     [else
      (script-scene->pict s scene)])))

