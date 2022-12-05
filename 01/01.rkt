#lang racket
(require "../lib/setup.rkt"
         "../lib/util.rkt"
         "title.rkt"
         "open-compiler.rkt"
         slideshow slideshow/play pict/shadow)

(provide introduction)

(define (introduction)
  (title->thesis)

  (slide
   (scale (vc-append
           10
           (tt "#define")
           (hbl-append 40 (t "cpp") (t "m4"))
           (blank 0 20)
           (colorize (t "Not these macros!") "red"))
          2))

  (slide
   (lt-superimpose
    (scale-to-fit (bitmap (build-path paper-images "aim-057.png"))
                  client-w client-h)
    (colorize (linewidth 8 (frame (inset (scale (t "From 1963") 1.5) 20 10))) "red")))

  (important-papers)
  
  (open-compiler))

(define (title->thesis)
  (define title (scale (t "Modern Macros") 2))
  (define title-phase1 (ghost (launder title)))
  (define title-phase2 (ghost (launder title)))
  (define phase1
    (cc-superimpose
     plt-title-background
     (vc-append
      title-phase1
      (blank 0 100)
      (vl-append
       (t "Robby Findler")
       (t "Northwestern University")))))
  (define phase2
    (vc-append
     20
     title-phase2
     (scale (t "are an") 2)
     (scale (t "Open Compiler") 2)))

  (play-n
   (λ (n1)
     (define before-n n1)
     (define after-n (- 1 n1))
     (slide-pict
      (cc-superimpose
       (cellophane phase1 (- 1 before-n))
       (cellophane phase2 (- 1 after-n)))
      title
      title-phase1
      title-phase2
      before-n))))

(define (important-papers)
  (define p1986 (paper 1986))
  (define p1990 (paper 1990))
  (define p1992 (paper 1992))
  (define p2002 (paper 2002))
  (define p2016 (paper 2016))

  (define (s p) (slide (scale-to-fit p client-w client-h)))

  (define p1986+p1990
    (lt-superimpose
     p1986
     (inset p1990
            (/ (pict-width p1986) 8)
            (/ (pict-height p1986) 8)
            0
            0)))
  
  (s p1986)
  (s p1986+p1990)
  (slide
   (vc-append
    60
    (t "Four Deep Technical Results Over 40 Years")
    (scale-to-fit (hc-append p1986+p1990 p1992 p2002 p2016)
                  client-w client-h))))

(define (slide-and-scale main p start finish n)
  (define-values (sl st) (lt-find main start))
  (define-values (sr sb) (rb-find main start))
  (define-values (fl ft) (lt-find main finish))
  (define-values (fr fb) (rb-find main finish))
  (define sw (- sr sl))
  (define fw (- fr fl))
  (define sh (- sb st))
  (define fh (- fb ft))
  (pin-over
   main
   (interpolate sl fl n)
   (interpolate st ft n)
   (scale p
          (/ (interpolate sw fw n) (pict-width p))
          (/ (interpolate sh fh n) (pict-height p)))))
         
(define (paper name)
  (define filename
    (cond
      [(number? name)
       (for/or ([file (in-list (directory-list paper-images))])
         (and (regexp-match? (~a "^" name) (path->bytes file))
              file))]
      [else name]))
  (shadow-frame
   (lt-superimpose
    (bitmap
     (build-path paper-images filename))
    (if (number? name)
        (scale (inset (colorize (t (~a name)) "red") 10) 3)
        (blank)))))

(module+ main (introduction))
