#lang racket
(require pict/code slideshow racket/runtime-path)
(provide paper-images)

(current-code-font '(bold . "Inconsolata"))
(current-tt-font '(bold . "Inconsolata"))
(current-main-font "Lato")
(define-runtime-path paper-images "paper-images")