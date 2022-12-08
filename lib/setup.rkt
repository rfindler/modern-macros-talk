#lang racket
(require slideshow/code slideshow racket/runtime-path)
(provide paper-images)

(current-code-font '(bold . "Inconsolata"))
(current-tt-font '(bold . "Inconsolata"))
(current-tt-font-size (+ (current-font-size) 6))
(current-main-font "Lato")
(define-runtime-path paper-images "../paper-images")
(get-current-code-font-size (λ () (current-tt-font-size)))
