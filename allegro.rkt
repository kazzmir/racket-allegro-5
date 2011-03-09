#lang racket/base

(require ffi/unsafe)

(define library-path (build-path "lib" "liballegro"))
(define liballegro (ffi-lib library-path))
(define image-addon (ffi-lib (build-path "lib" "liballegro_image")))
(define font-addon (ffi-lib (build-path "lib" "liballegro_font")))

(define-syntax allegro-function
  (syntax-rules (:)
    [(_ id : x ...)
     (let ()
       (define c-symbol  (format "al_~a" (regexp-replaces 'id '((#rx"-" "_")))))
       (define libraries (list liballegro image-addon font-addon))
       (or (ormap (lambda (library)
                    (get-ffi-obj c-symbol library (_fun x ...)
                                 (lambda () #f)))
                  libraries)
           (error 'allegro-function "Could not find ~a" 'id)))]))

(define-syntax define-allegro
  (syntax-rules (:)
    [(_ id : x ...) (define id (allegro-function id : x ...))]))

(define-syntax-rule (define-allegro* id rest ...)
                    (begin
                      (define-allegro id rest ...)
                      (provide id)))

(define ALLEGRO-VERSION (+ (arithmetic-shift 5 24)
                           (arithmetic-shift 0 16)
                           (arithmetic-shift 0 8)
                           0))

(define-cstruct _Display ([data _int]))
(define-cstruct _Font ([data _int]))
(define-cstruct _Bitmap ([data _int]))
(define-cstruct _Timer ([data _int]))
(define-cstruct _EventSource ([data _int]))
(define-cstruct _EventQueue ([data _int]))
(define-cstruct _Event ([type _int]
                        [source _pointer]
                        [timestamp _double]
                        [x1 _int] [x2 _int] [x3 _int]
                        [x4 _int] [x5 _int] [x6 _int]
                        [x7 _int] [x8 _int] [x9 _int]
                        [x10 _int] [x11 _int] [x12 _int]
                        ))

(define-allegro* install-system : (_int = ALLEGRO-VERSION) (_pointer = #f) -> _bool)
(define-allegro* init-image-addon : -> _bool)
(define-allegro* init-font-addon : -> _bool)

(define-allegro* create-display : _int _int -> _Display-pointer)
(define-allegro* install-keyboard : -> _bool)
(define-allegro* install-mouse : -> _bool)
(define-allegro* load-font : _string (_int = 0) (_int = 0) -> _Font-pointer)
(define-allegro* load-bitmap : _string -> _Bitmap-pointer)
(define-allegro* create-timer : _double -> _Timer-pointer)
(define-allegro* start-timer : _Timer-pointer -> _void)
(define-allegro* create-event-queue : -> _EventQueue-pointer)
(define-allegro* register-event-source : _EventQueue-pointer _EventSource-pointer -> _void)
(define-allegro* get-timer-event-source : _Timer-pointer -> _EventSource-pointer)
(define-allegro* get-keyboard-event-source : -> _EventSource-pointer)
(define-allegro* get-mouse-event-source : -> _EventSource-pointer)
(define-allegro* get-display-event-source : _Display-pointer -> _EventSource-pointer)
;; (define-allegro* get-next-event : _EventQueue-pointer (
(define-allegro* wait-for-event : _EventQueue-pointer (event : (_ptr o _Event)) -> _void -> event)
