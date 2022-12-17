#lang reactor

(define-signal input)

(define/contract (counter input chan)
  (reactive-> pure-signal? value-signal? none/c)
  (emit& chan 0)
  (loop&
   (await& #:immediate input)
   (emit& chan (add1 (last chan)))
   pause&))

(define/contract (printloop chan)
  (reactive-> value-signal? none/c)
  (loop&
   (await& chan
           [times
            (printf "got total of ~a inputs\n" times)])))

(define/contract (main input)
  (reactive-> pure-signal? none/c)
  (define-signal crosstalk 0 #:gather +)
  (par& (counter input crosstalk)
        (printloop crosstalk)))

(define r (prime main input))
(react! r)
(react! r)
(react! r input)
(react! r)
(react! r)
(react! r input)
(react! r)
