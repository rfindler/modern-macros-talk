#lang slideshow
(require "code.rkt"
         "card.rkt"
         "plan.rkt"
         "script.rkt"
         slideshow/play)

(provide scope-slides)

;; ----------------------------------------

(define (xslides l)
  (define all (launder
               (ghost (apply cc-superimpose
                             (filter pict? l)))))
  (for ([p (in-list l)])
    (cond
     [(pict? p)
      (slide (lt-superimpose all p))]
     [(middle-play? p)
      (play-n (lambda (t) (lt-superimpose all ((middle-play-p p) t))) #:skip-first? #t #:skip-last? #t)]
     [else
      (play-n (lambda (t) (lt-superimpose all (p t))) #:skip-last? #t)
      (play-n (lambda (t) (lt-superimpose all (p (- 1 t)))))])))

(define (scope-slides #:more-pedantic? [more-pedantic? #f]
                      #:just-or? [just-or? #f])
  (xslides
   (for/list ([p (in-list (if just-or?
                              (just-or-example)
                              (premade-or-example #f)))])
     (if (pict? p)
         (inset p 0 0 0 (* 4 gap-size))
         p)))

  (xslides
   (append
    (basic-lexical-scope)
    (for/list ([p (in-list (scopes-example #:more-pedantic? more-pedantic?))])
      (if (list? p)
          (lambda (t)
            (define red? ((length p) . > . 4))
            (define (reorder l)
              (if (and red? (t . > . 0.5))
                  (list* (list-ref l 3) (car l) (cadr l) (caddr l) 
                         (cellophane (list-ref l 3) (- 1.1 t))
                         (list-tail l 4))
                  l))
            (refocus (cc-superimpose
                      (ghost (first p))
                      (launder
                       (apply cc-superimpose
                              (reorder
                               (for/list ([y (in-list p)]
                                          [i (in-naturals)])
                                 (cellophane (tilt y (- i 2) t #f
                                                   #:rot-dir (if (and red?
                                                                      (= i 3))
                                                                 -1
                                                                 1))
                                             (- 1 (* t 0.25))))))))
                     (first p)))
          p))))

  (unless just-or?
    (xslides
     (premade-or-example #t))))

;; ----------------------------------------

(module+ main
  (scope-slides))
