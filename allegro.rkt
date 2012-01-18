#lang racket/base

(require ffi/unsafe racket/match
         racket/runtime-path
         (for-syntax racket/base
                     syntax/parse))

(define-runtime-path library-path (build-path "lib"))
(define liballegro (ffi-lib (build-path library-path "liballegro")))
(define image-addon (ffi-lib (build-path library-path "liballegro_image")))
(define font-addon (ffi-lib (build-path library-path "liballegro_font")))
(define ttf-addon (ffi-lib (build-path library-path "liballegro_ttf")))
(define primitives-addon (ffi-lib (build-path library-path "liballegro_primitives")))
(define acodec-addon (ffi-lib (build-path library-path "liballegro_acodec")))

(define-syntax allegro-function
  (syntax-rules (:)
    [(_ id : x ...)
     (let ()
       (define c-symbol  (format "al_~a" (regexp-replaces 'id '((#rx"-" "_")))))
       (define libraries (list liballegro image-addon font-addon primitives-addon
                               ttf-addon acodec-addon))
       (or (ormap (lambda (library)
                    (get-ffi-obj c-symbol library (_fun x ...)
                                 (lambda () #f)))
                  libraries)
           (error 'allegro-function "Could not find ~a" 'id)))]))

(define-syntax-rule (define-allegro id : x ...)
                    (define id (allegro-function id : x ...)))

(define-syntax-rule (define-allegro* id rest ...)
                    (begin
                      (define-allegro id rest ...)
                      (provide id)))

(define ALLEGRO-VERSION (+ (arithmetic-shift 5 24)
                           (arithmetic-shift 0 16)
                           (arithmetic-shift 5 8)
                           0))

;; Opaque datastructures
(define-cstruct _Display ([data _int]))
(define-cstruct _Font ([data _int]))
(define-cstruct _Joystick ([data _int]))
(define-cstruct _Bitmap ([data _int]))
(define-cstruct _Timer ([data _int]))
(define-cstruct _Sample ([data _int]))
(define-cstruct _EventSource ([data _int]))
(define-cstruct _EventQueue ([data _int]))
(define-cstruct _LockedRegion ([data _int]))

(define PlayMode
  (_enum '(Once = #x100
           Loop = #x101
           Bidir = #x102)))

(define KeyCodes
  (_enum '(A = 1
           B = 2
           C = 3
           D = 4
           E = 5
           F = 6
           G = 7
           H = 8
           I = 9
           J = 10
           K = 11
           L = 12
           M = 13
           N = 14
           O = 15
           P = 16
           Q = 17
           R = 18
           S = 19
           T = 20
           U = 21
           V = 22
           W = 23
           X = 24
           Y = 25
           Z = 26

           0 = 27
           1 = 28
           2 = 29
           3 = 30
           4 = 31
           5 = 32
           6 = 33
           7 = 34
           8 = 35
           9 = 36

           Pad0 = 37
           Pad1 = 38
           Pad2 = 39
           Pad3 = 40
           Pad4 = 41
           Pad5 = 42
           Pad6 = 43
           Pad7 = 44
           Pad8 = 45
           Pad9 = 46

           F1 = 47
           F2 = 48
           F3 = 49
           F4 = 50
           F5 = 51
           F6 = 52
           F7 = 53
           F8 = 54
           F9 = 55
           F10 = 56
           F11 = 57
           F12 = 58

           Escape = 59
           Tilde = 60
           Minus = 61
           Equals = 62
           Backspace = 63
           Tab = 64
           Openbrace = 65
           Closebrace = 66
           Enter = 67
           Semicolon = 68
           Quote = 69
           Backslash = 70
           Backslash2 = 71
           Comma = 72
           Fullstop = 73
           Slash = 74
           Space = 75

           Insert = 76
           Delete = 77
           Home = 78
           End = 79
           Pgup = 80
           Pgdn = 81
           Left = 82
           Right = 83
           Up = 84
           Down = 85

           PadSlash = 86
           PadAsterisk = 87
           PadMinus = 88
           PadPlus = 89
           PadDelete = 90
           PadEnter = 91
           Printscreen = 92
           Pause = 93

           AbntC1 = 94
           Yen = 95
           Kana = 96
           Convert = 97
           Noconvert = 98
           At = 99
           Circumflex = 100
           Colon2 = 101
           Kanji = 102
           PadEquals = 103
           Backquote = 104
           Semicolon2 = 105
           Command = 106
           Unknown = 107

           Modifiers = 215
           Lshift = 215
           Rshift = 216
           Lctrl = 217
           Rctrl = 218
           Alt = 219
           Altgr = 220
           Lwin = 221
           Rwin = 222
           Menu = 223
           Scrolllock = 224
           Numlock  = 225
           Capslock = 226
           Max)))

(define TextFlags
  (_enum '(AlignLeft = 0
           AlignCenter = 1
           AlignRight = 2)))

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

(define PixelFormat
  (_enum '(Any = 0
           AnyNoAlpha
           AnyWithAlpha
           Any15NoAlpha
           Any16NoAlpha
           Any16WithAlpha
           Any24NoAlpha
           Any32NoAlpha
           Any32WithAlpha
           ARGB8888
           RGBA8888
           ARGB4444
           RGB888
           RGB565
           RGB555
           RGBA5551
           ARGB1555
           ABGR8888
           XBGR8888
           BGR888
           BGR565
           BGR555
           RGBX8888
           XRGB8888
           ABGRF32
           ABGR8888LE
           RGBA4444
           Luminance8)))

(define LockType
  (_enum '(ReadWrite = 0
           ReadOnly = 1
           WriteOnly = 2)))

;; automatically converts whatever to a float by adding 0.0
(define-fun-syntax _float*
  (syntax-id-rules (_float*)
    [_float* (type: _float pre: (x => (+ 0.0 x)))]))

(define-fun-syntax _int*
  (syntax-id-rules (_int*)
    [_float* (type: _int pre: (x => (inexact->exact (round x))))]))

(define-fun-syntax _double**
  (syntax-id-rules (_double**)
    [_double** (type: _double pre: (x => (+ 0.0 x)))]))

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
                                [keycode KeyCodes]
                                [unicode _int]
                                [modifiers _uint]
                                [repeat _bool]))
(define-event-matcher KeyboardEvent type source timestamp display keycode
                                    unicode modifiers repeat)

(define-cstruct _TimerEvent ([type EventType]
                             [source _Timer-pointer]
                             [timestamp _double]
                             [count _int64]
                             [error _double]))
(define-event-matcher TimerEvent type source timestamp count error)

(define-cstruct _MouseEvent ([type EventType]
                             [source _pointer]
                             [timestamp _double]
                             [display _Display-pointer]
                             [x _int] [y _int] [z _int] [w _int]
                             [dx _int] [dy _int] [dz _int] [dw _int]
                             [button _uint]
                             [pressure _float]))
(define-event-matcher MouseEvent type source timestamp display x y z w 
                                 dx dy dz dw button pressure )

(define-cstruct _JoystickEvent ([type EventType]
                                [source _pointer]
                                [timestamp _double]
                                [id _Joystick-pointer]
                                [stick _int]
                                [axis _int]
                                [position _float]
                                [button _int]))
(define-event-matcher JoystickEvent type source timestamp id stick axis position button)

(define-cstruct _DisplayEvent ([type EventType]
                               [source _pointer]
                               [timestamp _double]
                               [x _int] [y _int]
                               [width _int] [height _int]
                               [orientation _int]))
(define-event-matcher DisplayEvent type source timestamp x y width height orientation)

(provide (rename-out [match:KeyboardEvent KeyboardEvent]
                     [match:TimerEvent TimerEvent]
                     [match:MouseEvent MouseEvent]
                     [match:JoystickEvent JoystickEvent]
                     [match:DisplayEvent DisplayEvent]
                     ))

(define-cstruct _Color ([red _float]
                        [green _float]
                        [blue _float]
                        [alpha _float]))

(define-allegro* install-system : (_int = ALLEGRO-VERSION) (_pointer = #f) -> _bool)
(define-allegro* uninstall-system : -> _void)
(define-allegro* init-image-addon : -> _bool)
(define-allegro* init-primitives-addon : -> _bool)
(define-allegro* init-ttf-addon : -> _bool)
(define-allegro* init-font-addon : -> _bool)
(define-allegro* init-acodec-addon : -> _bool)

(define-allegro* create-display : _int _int -> _Display-pointer)
(define-allegro* get-current-display : -> _Display-pointer)
(define-allegro* install-keyboard : -> _bool)
(define-allegro* install-mouse : -> _bool)
(define-allegro* get-bitmap-width : _Bitmap-pointer -> _int)
(define-allegro* get-bitmap-height : _Bitmap-pointer -> _int)
(define-allegro* draw-scaled-rotated-bitmap : _Bitmap-pointer _float* _float* _float* _float* _float* _float* _float* _int -> _void)
(define-allegro* load-font : _string _int _int -> _Font-pointer)
(define-allegro* load-bitmap : _string -> _Bitmap-pointer)
(define-allegro* draw-bitmap : _Bitmap-pointer _float* _float* _int -> _void)
(define-allegro* destroy-bitmap : _Bitmap-pointer -> _void)
(define-allegro* convert-mask-to-alpha : _Bitmap-pointer _Color -> _void)
(define-allegro* draw-text : _Font-pointer _Color _float* _float* TextFlags _string -> _void)
(define-allegro* create-timer : _double** -> _Timer-pointer)
(define-allegro* start-timer : _Timer-pointer -> _void)
(define-allegro* stop-timer : _Timer-pointer -> _void)
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
(define-allegro* flip-display : -> _void)
(define-allegro* map-rgb-f : _float* _float* _float* -> _Color)
(define-allegro* unmap-rgb-f : _Color (red : (_ptr o _float))
                                      (green : (_ptr o _float))
                                      (blue : (_ptr o _float)) -> _void ->
                                      (values red green blue))

(define-allegro* unmap-rgba-f : _Color (red : (_ptr o _float))
                                       (green : (_ptr o _float))
                                       (blue : (_ptr o _float))
                                       (alpha : (_ptr o _float))
                                       -> _void ->
                                       (values red green blue alpha))

(define-allegro* map-rgba-f : _float* _float* _float* _float* -> _Color)
(define-allegro* clear-to-color : _Color -> _void)
(define-allegro* draw-tinted-bitmap : _Bitmap-pointer _Color _float* _float* _float* -> _void)

(define-allegro* lock-bitmap : _Bitmap-pointer [PixelFormat = 'Any] [LockType = 'ReadWrite] -> _LockedRegion-pointer)
(define-allegro* unlock-bitmap : _Bitmap-pointer -> _void)

(define-allegro* is-event-queue-empty : _EventQueue-pointer -> _bool)
(define-allegro* get-display-width : _Display-pointer -> _int)
(define-allegro* get-display-height : _Display-pointer -> _int)
(define-allegro* draw-filled-triangle : _float* _float* _float* _float* _float* _float* _Color -> _void)
(define-allegro* draw-filled-circle : _float* _float* _float* _Color -> _void)
(define-allegro* draw-filled-rectangle : _float* _float* _float* _float* _Color -> _void)
(define-allegro* draw-filled-rounded-rectangle : _float* _float* _float* _float* _float* _float* _Color -> _void)
(define-allegro* put-pixel : _int* _int* _Color -> _void)
(define-allegro* get-backbuffer : _Display-pointer -> _Bitmap-pointer)

(define-allegro* install-audio : -> _void)
(define-allegro* reserve-samples : _int -> _void)
(define-allegro* load-sample : _string -> _Sample-pointer)
(define-allegro* play-sample : _Sample-pointer [_float* = 1.0] [_float* = 0.0] [_float* = 1.0] [PlayMode = 'Once] [_pointer = #f] -> _bool)

(provide (rename-out [ptr-equal? same-pointer]))

(provide setup-audio)
(define (setup-audio)
  (install-audio)
  (init-acodec-addon)
  (reserve-samples 8))
