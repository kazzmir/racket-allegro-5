#lang racket/base

(require ffi/unsafe)

(define library-path (build-path "lib" "liballegro"))
(define liballegro (ffi-lib library-path))

(define-syntax allegro-function
  (syntax-rules (:)
    [(_ id : x ...)
     (get-ffi-obj (format "al_~a" (regexp-replaces 'id '((#rx"-" "_"))))
                  liballegro (_fun x ...)) ]))

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

(define-allegro* install-system : (_int = ALLEGRO-VERSION) (_pointer = #f) -> _bool)
