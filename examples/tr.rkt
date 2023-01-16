#lang typed/racket

(: wc (-> Path-String Natural))
(define (wc fn)
  (call-with-input-file fn
    (λ ([port : Input-Port])
      (for/sum : Natural ([l (in-lines port)])
        (define words (regexp-split #px"\\s+" l))
        (length (remove* '("") words))))))
