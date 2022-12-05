#lang racket

(require (for-syntax syntax/parse racket)
         slideshow
         slideshow/code
         slideshow/play
         "../lib/setup.rkt")

(provide enum-slides)

(let ()
  (define-syntax animals
    (set 'dog 'turtle 'sheep 'horse))

  (define-syntax (animal-case stx)
    (syntax-case stx ()
      [(_ animal-expr [(an-animal ...) expr] ...)
       (unless (equal? (list->set (syntax->datum #'(an-animal ... ...)))
                       (syntax-local-value #'animals))
         (raise-syntax-error
          'animal-case
          "mismatched animals"
          stx))
       #'(case animal-expr [(an-animal ...) expr] ...)]))

  (animal-case 'x
               [(dog) 1]
               [(turtle) 2]
               [(sheep) 3]
               [(horse) 4])
  
  (void))

(let ()
  (define-syntax (define-enum stx)
    (syntax-parse stx
      [(_ enum-name:id enum-content:id ...)
       #'(define-syntax enum-name (set 'enum-content ...))]))
  
  (define-syntax (enum-case stx)
    (syntax-parse stx
      [(_ enum-name:id
          enum-value:expr
          [(enum-content ...) expr] ...)
       (unless (equal? (list->set (syntax->datum #'(enum-content ... ...)))
                       (syntax-local-value #'enum-name (λ () #f)))
         (raise-syntax-error
          'enum-case
          "mismatched enum"
          stx #'enum-name))
       #'(case enum-value
           [(enum-content ...) expr] ...)]))

  (void))

(define-syntax (save-code stx)
  (syntax-parse stx
    [(_ x expr)
     (syntax-local-lift-expression #'(set! x (code expr)))
     #'expr]))

(define (case-version-pict turtle? animal-case?)
  (code
   (define (animal-speed an-animal)
     (case an-animal
       [(dog) 2]
       #,(cellophane (code [(turtle) 0.01])
                     (if turtle? 1 .1))
       [(sheep) 1]
       [(horse) 3]))))

(define animal-enum-pict
  (code (define-enum animals
          dog turtle sheep horse)))
(define (enum-version-pict turtle?)
  (code
   (define (animal-speed an-animal)
     (case animals
       [(dog) 2]
       #,(cellophane (code [(turtle) 0.01])
                     (if turtle? 1 .1))
       [(sheep) 1]
       [(horse) 3]))))

(define (case-badness case-v enum-v turtle? error-v)
  (vl-append
   (case-v (t "Forget one animal:"))
   (ht-append
    (blank 60 0)
    (vl-append
     (blank 0 20)
     (enum-version-pict turtle?)
     (blank 0 20)))
   (error-v (colorize (tt "no error!") "red"))))

(define (ds-slv ds slv)
  (slide
   (vl-append
    60
    (t "Key capability:")

    (ht-append
     (blank 40 0)
     (lt-superimpose
      (ds (code
           (define-syntax animals
             (set 'dog 'turtle 'sheep 'horse))))
      
      (slv (code
            (syntax-local-value #'animals)
            =
            (set 'dog 'turtle 'sheep 'horse)))))

    (lt-superimpose
     (ds (para (code define-syntax)
               "binds arbitrary things for compile-time, not just macros"))
     (slv
      (para (code syntax-local-value)
            "looks up things bound by" (code define-syntax)))))))

(define (enum-slides)
  (slide (case-badness ghost ghost #t ghost))
  (slide (case-badness values values #f values))

  (ds-slv values ghost)
  (ds-slv ghost values)

  (play-n
   (λ (n1 n2)

     (define same-defn
       (code
        (define same-animals?
          (same-as-set?
           #'(an-animal ... ...)
           (syntax-local-value #'animals)))))
     (define same-check
       (code
        (unless same-animals?
          (raise-syntax-error
           'animal-case "mismatched animals" stx))))

     (define (slide-out p n)
       (inset/clip p
                   0 (* (- 1 n) (pict-height p) -1/2)
                   0 (* (- 1 n) (pict-height p) -1/2)))
     
     (code
      (define-syntax (animal-case stx)
        (syntax-parse stx
          [(animal-case animal-expr
                        [(an-animal ...) expr] ...)
           #,(slide-out (vl-append (ghost (code "")) same-defn) n1)
           #,(ghost (code ""))
           #,(slide-out (vl-append same-check (ghost (code ""))) n2)
           #'(case animal-expr
               [(an-animal ...) expr] ...)])))))
  
  (slide
   (vl-append
    40
    animal-enum-pict
    (tt "⇒")
    (code
     (define-syntax animals
       (set 'dog 'turtle 'sheep 'horse)))))
  
  (slide
   (code
    (define-syntax (define-enum stx)
      (syntax-parse stx
        [(_ enum-name enum-cases ...)
         #'(define-syntax enum-name
             (set 'enum-cases ...))]))))

  (define (enum-case-slide highlight?)

    (define (highlight txt p)
      (cond
        [highlight?
         (define p (colorize (tt txt) "white"))
         (refocus (cc-superimpose
                   (colorize
                    (filled-rounded-rectangle (+ (pict-width p) 30)
                                              (+ (pict-height p) 10))
                    "black")
                   p)
                  p)]
        [else p]))
    
    (slide
     (code
      (define-syntax (enum-case stx)
        (syntax-parse stx
          [(enum-case #,(highlight "enum-id" (code enum-id)) enum-expr
             [(an-item ...) expr] ...)
           #,(ghost (code ""))
           (define same-enums?
             (same-as-set?
              #'(an-item ... ...)
              (syntax-local-value #,(highlight "#'enum-id" (code #'enum-id)))))
           #,(ghost (code ""))
           (unless same-enum?
             (raise-syntax-error
              'enum-case "mismatched elements" stx))
           #,(ghost (code ""))
           #'(case enum-expr
               [(an-item ...) expr] ...)])))))

  (enum-case-slide #f)
  (enum-case-slide #t)

  (slide
   (vl-append
    20
    (code
     (define-enum animals
       dog turtle sheep horse))
    
    (code
     (define (animal-speed an-animal)
       (enum-case animals an-animal
         [(dog) 2]
         [(sheep) 1]
         [(horse) 3])))
    
    (colorize
     (text "enum-case: mismatched elements"
           (cons 'italic (current-code-font))
           (current-font-size))
     "red"))))

(module+ main (enum-slides))
