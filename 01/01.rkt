#lang racket
(require "../setup.rkt" slideshow "open-compiler.rkt"
         pict/shadow)

(define (introduction)
  (slide
   (scale (vc-append
           10
           (t "Modern Macros")
           (t "as an")
           (t "Open Compiler"))
          2))

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
