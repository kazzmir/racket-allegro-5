#lang racket/base

;; This is the same example program from allegro5/python/ex_draw_bitmap.py

(require (prefix-in allegro: "allegro.rkt") racket/match)

(define (main)
(define width 640)
(define height 480)
(define FPS 60)
(printf "Install Allegro: ~a\n" (allegro:install-system))
(printf "Install Image Addon: ~a\n" (allegro:init-image-addon))
(printf "Install Font Addon: ~a\n" (allegro:init-font-addon))
(printf "Install keyboard: ~a\n" (allegro:install-keyboard))
(printf "Install mouse: ~a\n" (allegro:install-mouse))

(define display (allegro:create-display width height))
(define font (allegro:load-font (build-path "data" "fixed_font.tga") 10 0))
(define mysha (allegro:load-bitmap (build-path "data" "mysha256x256.png")))
(define timer (allegro:create-timer (/ 1.0 FPS)))
(define queue (allegro:create-event-queue))
(allegro:register-event-source queue (allegro:get-keyboard-event-source))
(allegro:register-event-source queue (allegro:get-mouse-event-source))
(allegro:register-event-source queue (allegro:get-timer-event-source timer))
(allegro:register-event-source queue (allegro:get-display-event-source display))
(allegro:start-timer timer)

(define-struct sprite (x y dx dy))

(define (draw sprites image)
  (define white (allegro:map-rgb-f 1.0 1.0 1.0))
  (allegro:clear-to-color (allegro:map-rgb-f 0.0 0.0 0.0))
  (for ([sprite sprites])
    (allegro:draw-tinted-bitmap image white
                                ;; (+ 0.0 (sprite-x sprite))
                                (sprite-x sprite)
                                (sprite-y sprite)
                                0))
  (allegro:flip-display))

(define (create-sprite display fps)
  (define width (allegro:get-display-width display))
  (define height (allegro:get-display-height display))
  (define x (random width))
  (define y (random height))
  (define angle (random 360))
  (define dx (* 2 fps (cos (* 3.14159 (/ angle 180)))))
  (define dy (* 2 fps (sin (* 3.14159 (/ angle 180)))))
  (sprite x y dx dy))

(define (update sprites display fps)
  (define width (allegro:get-display-width display))
  (define height (allegro:get-display-height display))
  (define bitmap-size 256)
  (define (less-than-zero x) (< x 0))
  (define (too-large-x x) (> (+ x bitmap-size) width))
  (define (too-large-y y) (> (+ y bitmap-size) height))
  (define new sprite)
  (for/list ([sprite sprites])
    (define x (+ (/ (sprite-dx sprite) fps) (sprite-x sprite)))
    (define dx (sprite-dx sprite))
    (match x
      [(? less-than-zero) (set! x (- x))
                          (set! dx (- dx))]
      [(? too-large-x) (set! x (+ (- x) (* 2 (- width bitmap-size))))
                     (set! dx (- dx))]
      [else (void)])

    (define y (+ (/ (sprite-dy sprite) fps) (sprite-y sprite)))
    (define dy (sprite-dy sprite))
    (match y
      [(? less-than-zero) (set! y (- y))
                          (set! dy (- dy))]
      [(? too-large-y) (set! y (+ (- y) (* 2 (- height bitmap-size))))
                     (set! dy (- dy))]
      [else (void)])
    (new x y dx dy)))

(define sprites (list (create-sprite display FPS)))
(let loop ([redraw? #f])
  (if (and redraw?
           (allegro:is-event-queue-empty queue))
    (begin
      (draw sprites mysha)
      (loop #f))
    (let ()
      (define event (allegro:wait-for-event queue))
      (match event
        [(allegro:KeyboardEvent type source timestamp display keycode unicode modifiers repeat)
         (match type
           ['KeyChar (case keycode
                       [(Escape)
                        (printf "Quit!\n")]
                       [else (loop redraw?)])]
           [else (loop redraw?)])]
        [(allegro:TimerEvent type source timestamp count error)
         (set! sprites (update sprites display FPS))
         (loop #t)]
        [else (printf "unknown event\n")
              (loop redraw?)]))))
)

(allegro:run main)
