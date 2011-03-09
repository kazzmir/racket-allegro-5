#lang racket/base

(require (prefix-in allegro: "allegro.rkt"))

(define width 640)
(define height 480)
(define FPS 60)
(define (racket->c-double n) (+ n 0.0))
(printf "Install Allegro: ~a\n" (allegro:install-system))
(printf "Install Image Addon: ~a\n" (allegro:init-image-addon))
(printf "Install Font Addon: ~a\n" (allegro:init-font-addon))
(allegro:install-keyboard)
(allegro:install-mouse)

(define display (allegro:create-display width height))
(define font (allegro:load-font (build-path "data" "fixed_font.tga")))
(define mysha (allegro:load-bitmap (build-path "data" "mysha256x256.png")))
(define timer (allegro:create-timer (racket->c-double (/ 1.0 FPS))))
(define queue (allegro:create-event-queue))
(allegro:register-event-source queue (allegro:get-keyboard-event-source))
(allegro:register-event-source queue (allegro:get-mouse-event-source))
(allegro:register-event-source queue (allegro:get-timer-event-source timer))
(allegro:register-event-source queue (allegro:get-display-event-source display))
(allegro:start-timer timer)

(let loop ()
  (define event (allegro:wait-for-event queue))
  (printf "Got an event! ~a\n" event)
  (loop))
