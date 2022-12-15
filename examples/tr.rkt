#lang typed/racket

(: wc (-> Path-String Natural))
(define (wc fn)
  (call-with-input-file fn
    (λ ([port : Input-Port])
      (for/sum : Natural ([l (in-lines port)])
        (define words (regexp-split #px"\\s+" l))
        (length (remove* '("") words))))))

(module+ test
  (require typed/rackunit)
  (call-with-output-file "wc.txt"
    (λ ([port : Output-Port])
      (displayln "a b c" port)
      (displayln "   dee\teee\t eff  " port)
      (displayln "" port))
    #:exists 'truncate)
  (check-equal? (wc "wc.txt") 6))
