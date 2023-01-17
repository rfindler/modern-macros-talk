#lang racket
(require "../lib/setup.rkt"
         "../lib/title.rkt"
         slideshow slideshow/play)

(provide thanks)
(define (thanks)
  (define bkg
    (inset (colorize (filled-rectangle (+ client-w margin margin) (+ client-h margin margin))
                     plt-background-color)
           (- margin)))
  (play-n
   #:steps (cons 30 10)
   (λ (n2-n3 n4)
     (define-values (n2 n3) (split-phase n2-n3))
     (define sorawee (mk-pict+string "Sorawee Porncharoenwase"))
     (cc-superimpose
      (refocus (lt-superimpose
                bkg
                (inset plt-title-background 400 300 0 0))
               bkg)
      (hc-append
       60
       (scale
        (vc-append
         4
         (vc-append -4
                    (t "Modern")
                    (t "Macros"))
         (t "are an")
         (vc-append -4
                    (t "Open")
                    (t "Compiler")))
        3)

       (cellophane
        (hide-left
         (add-background-on-name
          (vl-append
           60
           (folks "Macro Paper Authors"
                  "Matthew Flatt" "Matthias Felleisen"
                  "Daniel P. Freidman" "Bruce Duba"
                  "William Clinger" "Jonathan Rees"
                  "R. Kent Dybvig" "Robert Hieb"
                  "Carl Bruggeman" "Mitchell Wand"
                  "Eugene E. Kohlbecker"
                  "Ryan Culpepper" "Michael Ballantyne"
                  "Alexis King" "Timothy P. Hart")
           (folks "Language Authors"
                  sorawee
                  "Jesse Tov" "Spencer Florence"
                  "Stephen Chang" "Alex Knauth"
                  "Sam Tobin-Hochstadt"))
          sorawee n4)
         n2)
        n3))))))

(define (hide-left p n)
  (clip (inset p 0 0 (* (- 1 n) (- (pict-width p))) 0)))

(define (add-background-on-name main p+s n)
  (define p (t (pict+string-string p+s)))
  (define w (+ (pict-width p) 20))
  (define h (+ (pict-height p) 6))
  (pin-under
   (pin-over
    main
    (pict+string-pict p+s)
    lt-find
    (lt-superimpose (cellophane p (- 1 n))
                    (cellophane (colorize p "white") n)))
   (pict+string-pict p+s)
   lt-find
   (cellophane
    (refocus (cc-superimpose (inset (colorize (filled-rounded-rectangle w h) "firebrick")
                                    0 0 0 -2)
                             (ghost p))
             p)
    n)))

(struct pict+string (pict string))
(define (mk-pict+string string)
  (pict+string
   (ghost (t string))
   string))

(define (folks caption . names)
  (set! names (sort names string<? #:key last-name))
  (set! names (add-and names))
  (vl-append
   (scale (t caption) 2)
   (inset (apply para
                 #:width 680
                 (add-between (map pict+string->pict names) ",")) 40 0 0 0)))

(define (pict+string->pict p)
  (cond
    [(pict+string? p) (pict+string-pict p)]
    [else p]))

(define (last-name str)
  (when (pict+string? str) (set! str (pict+string-string str)))
  (last (regexp-split #rx" " str)))

(define (add-and names)
  (let loop ([names names])
    (cond
      [(null? (cdr names))
       (list (~a "and " (car names)))]
      [else (cons (car names) (loop (cdr names)))])))


(module+ main (thanks))
