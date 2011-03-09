#lang racket/base

;; This is the same example program from allegro5/python/ex_draw_bitmap.py

(require (prefix-in allegro: "allegro.rkt") racket/match)

(define width 640)
(define height 480)
(define FPS 60)
(define (racket->c-double n) (+ n 0.0))
(printf "Install Allegro: ~a\n" (allegro:install-system))
(printf "Install Image Addon: ~a\n" (allegro:init-image-addon))
(printf "Install Font Addon: ~a\n" (allegro:init-font-addon))
(printf "Install keyboard: ~a\n" (allegro:install-keyboard))
(printf "Install mouse: ~a\n" (allegro:install-mouse))

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

(define (draw sprite)
  (define white (allegro:map-rgb-f 1.0 1.0 1.0))
  (allegro:clear-to-color (allegro:map-rgb-f 0.0 0.0 0.0))
  (allegro:draw-tinted-bitmap sprite white 100.0 100.0 0.0)
  (allegro:flip-display))

(let/ec quit
        (let loop ()
          (define event (allegro:wait-for-event queue))
          (match event
            [(allegro:KeyboardEvent type source timestamp display keycode unicode modifiers repeat)
             (match type
               ['KeyChar (case keycode
                           [(Escape)
                            (printf "Quit!\n")
                            (quit)])]
               [else (void)])]
             [(allegro:TimerEvent type source timestamp count error)
              (draw mysha)]
             [else (printf "unknown event\n")])
            (loop)))
