#lang racket/base

(require (prefix-in allegro: "allegro.rkt"))

(define width 640)
(define height 480)
(printf "Install: ~a\n" (allegro:install-system))
