#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require "interp-R0.rkt")
(require "interp-R1.rkt")
(require "interp.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; _next-unqiue : Integer
(define _next-unqiue 0)

;; next-unique-symbol :: Symbol
(define (next-unique-symbol)
  (define next (string->symbol (string-append "_" (number->string _next-unqiue))))
  (set! _next-unqiue (+ _next-unqiue 1))
  next)

;; maps old symbols in the source code to new unique symbols 
;; unique-symbol-table : HashMap Symbol Symbol
(define old-to-unique-symbol-table (make-hash))

;; uniquify-exp : R1 -> R1
(define (uniquify-exp e)
  (match e
    [(? symbol?)
      (hash-ref old-to-unique-symbol-table e)]
    [(? integer?) e]
    [`(let ([,x ,bnd]) ,bdy)
      (define bnd+ (uniquify-exp bnd))
      (define maybe-shadowed-var (hash-ref old-to-unique-symbol-table x null))
      (define next-var (next-unique-symbol))
      (hash-set! old-to-unique-symbol-table x next-var)
      (define bdy+ (uniquify-exp bdy))
      (hash-set! old-to-unique-symbol-table x maybe-shadowed-var)
      `(let ([,next-var ,bnd+]) ,bdy+)]
    [`(,op ,es ...)
      `(,op
        ,@(for/list
          ([e es])
          (uniquify-exp e)))]
    ))

;; uniquify : R1 -> R1
(define (uniquify p)
  (match p
    [`(program ,info ,e)
     `(program ,info ,(uniquify-exp e))]
    ))

(define my-test
  `(let ([my-var 42])
        (let ([input (read)])
             (let ([my-var (+ my-var (- input))])
                  my-var))))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  p)

;; explicate-control : R1 -> C0
(define (explicate-control p)
  p)

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  p)

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  p)

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  p)

;; print-x86 : x86 -> string
(define (print-x86 p)
  "todo - implement print-x86")
