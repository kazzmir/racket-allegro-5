#lang racket/base

(require ffi/unsafe racket/match
         (for-syntax racket/base
                     syntax/parse))

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

(define EventType
  (_enum '(JoystickAxis = 1
           JoystickButtonDown = 2
           JoystickButtonUp = 3
           JoystickConfiguration = 4

           KeyDown = 10
           KeyChar = 11
           KeyUp = 12

           MouseAxes = 20
           MouseButtonDown = 21
           MouseButtonUp = 22
           MouseEnterDisplay = 23
           MouseLeaveDisplay = 24
           MouseWarped = 25

           Timer = 30

           DisplayExpose = 40
           DisplayResize = 41
           DisplayClose = 42
           DisplayLost = 43
           DisplayFound = 44
           DisplaySwitchIn = 45
           DisplaySwithOut = 46
           DisplayOrientation = 47)))

;; matches a c-struct and binds all the fields
(define-for-syntax (event-matcher stx struct-fields name kind?)
  (syntax-parse stx
    [(_ fields ...)
     (when (not (= (length struct-fields)
                   (length (syntax->list #'(fields ...)))))
       (error 'event-matcher  "require ~a fields but given ~a"
              (length struct-fields) (length (syntax->list #'(fields ...)))))
     (with-syntax ([(real-fields ...)
                    (for/list ([field struct-fields])
                      (string->symbol (format "~a-~a" name field)))]
                   [kind? kind?])
       #'(and (? kind?)
              (app (lambda (event)
                     (real-fields event))
                   fields)
              ...))]))

;; defines a match-expander for a given c-struct
;; implicitly defines a matcher with the prefix `match:' on the name given
(define-syntax (define-event-matcher stx)
  (syntax-parse stx
    [(_ name fields ...)
     (with-syntax ([string-name (symbol->string (syntax->datum #'name))]
                   [id? (string->symbol (format "~a?" (syntax->datum #'name)))]
                   [matcher (datum->syntax #'name (string->symbol
                                                    (format "match:~a" (syntax->datum #'name)))
                                           #'name)])
       #'(define-match-expander matcher
                                (lambda (stx)
                                  (event-matcher stx '(fields ...)
                                                 string-name #'id?))))]))

;; the c struct is 60 bytes. a little more probably won't hurt too much
(define-cstruct _Event ([type EventType]
                        [source _pointer]
                        [timestamp _double]
                        [x1 _int] [x2 _int] [x3 _int]
                        [x4 _int] [x5 _int] [x6 _int]
                        [x7 _int] [x8 _int] [x9 _int]
                        [x10 _int] [x11 _int] [x12 _int]
                        [x13 _int] [x14 _int] [x15 _int]
                        ))

;; the cstructs and the matchers could be defined at once to alleviate the need
;; to repeat the field names
(define-cstruct _KeyboardEvent ([type EventType]
                                [source _pointer]
                                [timestamp _double]
                                [display _Display-pointer]
                                [keycode _int]
                                [unicode _int]
                                [modifiers _uint]
                                [repeat _bool]))
(define-event-matcher KeyboardEvent type source timestamp display keycode
                                    unicode modifiers repeat)

(define-cstruct _TimerEvent ([type EventType]
                             [source _pointer]
                             [timestamp _double]
                             [count _int64]
                             [error _double]))
(define-event-matcher TimerEvent type source timestamp count error)

(provide (rename-out [match:KeyboardEvent KeyboardEvent]
                     [match:TimerEvent TimerEvent]))


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
(define-allegro* wait-for-event : _EventQueue-pointer (event : (_ptr o _Event)) -> _void ->
                 (begin
                   (case (Event-type event)
                     [(Timer) (cpointer-push-tag! event TimerEvent-tag)]
                     [(KeyDown KeyChar KeyUp)
                      (cpointer-push-tag! event KeyboardEvent-tag)])
                   event))
