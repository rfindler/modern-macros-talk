#lang racket
(require "../setup.rkt" "open-compiler.rkt" "../title.rkt"
         slideshow slideshow/play pict/shadow)

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

  (slide (scale-to-fit (paper 1986) client-w client-h))

  (slide (scale-to-fit (vc-append (hc-append (paper 1986)
                                             (paper 1990)
                                             (paper 1992))
                                  (hc-append (paper 2002)
                                             (paper 2016)))
                       client-w client-h))
  
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

(define (paper name)
  (define filename
    (cond
      [(number? name)
       (for/or ([file (in-list (directory-list paper-images))])
         (and (regexp-match? (~a "^" name) (path->bytes file))
              file))]
      [else name]))
  (shadow-frame (bitmap (build-path paper-images filename))))

(module+ main (introduction))
