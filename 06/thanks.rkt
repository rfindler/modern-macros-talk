#lang racket
(require "../lib/setup.rkt"
         "../lib/title.rkt"
         slideshow)

(provide thanks)
(define (thanks)
  (define bkg
    (inset (colorize (filled-rectangle (+ client-w margin margin) (+ client-h margin margin))
                     plt-background-color)
           (- margin)))
  (slide
   (cc-superimpose
    (refocus (lt-superimpose
              bkg
              (inset plt-title-background 400 300 0 0))
             bkg)
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
            "Sorawee Porncharoenwase"
            "Jesse Tov" "Spencer Florence"
            "Stephen Chang" "Alex Knauth"
            "Sam Tobin-Hochstadt")))))

(define (folks caption . names)
  (set! names (sort names string<? #:key last-name))
  (set! names (add-and names))
  (vl-append
   (scale (t caption) 2)
   (inset (apply para
                 #:width 680
                 (add-between names ",")) 40 0 0 0)))

(define (last-name str)
  (last (regexp-split #rx" " str)))

(define (add-and names)
  (let loop ([names names])
    (cond
      [(null? (cdr names)) (list (~a "and " (car names)))]
      [else (cons (car names) (loop (cdr names)))])))


(module+ main (thanks))
