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

;;-------------------------------------------------------------------------------

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

(define uniquify-test
  `(let ([my-var 42])
        (let ([input (read)])
             (let ([my-var (+ my-var (- input))])
                  my-var))))

;;-------------------------------------------------------------------------------

;; complex-opera? : R1 -> Boolean
(define (complex-opera? e)
  (match e
    [(? symbol?) #f]
    [(? integer?) #f]
    [else #t]))

;; simplify an operator with one argument
;; simplify-operator1 : Operator -> R1 -> R1
(define (simplify-operator1 op arg)
  (match (complex-opera? arg)
    ; assign arg to new var and wrap operator in let
    [#t
     (define next-var (next-unique-symbol))
     (define arg+ (remove-complex-opera-exp* arg))
     `(let ([,next-var ,arg+]) (,op ,next-var))]
    ; just return operator with simple arg
    [else `(,op ,arg)]))

;; simplify an operator with two arguments
;; simplify-operator1 : Operator -> R1 -> R1 -> R1
(define (simplify-operator2 op arg1 arg2)
  (match* ((complex-opera? arg1) (complex-opera? arg2))
    [(#t #t)
     (define next-var1 (next-unique-symbol))
     (define arg1+ (remove-complex-opera-exp* arg1))
     (define next-var2 (next-unique-symbol))
     (define arg2+ (remove-complex-opera-exp* arg2))
     `(let ([,next-var1 ,arg1+]) (let ([,next-var2 ,arg2+]) (,op ,next-var1 ,next-var2)))]
    [(#t #f)
     (define next-var (next-unique-symbol))
     (define arg1+ (remove-complex-opera-exp* arg1))
     `(let ([,next-var ,arg1+]) (,op ,next-var ,arg2))]
    [(#f #t)
     (define next-var (next-unique-symbol))
     (define arg2+ (remove-complex-opera-exp* arg2))
     `(let ([,next-var ,arg2+]) (,op ,arg1 ,next-var))]
    [(#f #f)
     `(,op ,arg1 ,arg2)]))

;; remove-complex-opera-exp* : R1 -> R1
(define (remove-complex-opera-exp* e)
  (match e
    [(? symbol?) e]
    [(? integer?) e]
    [`(let ([,v ,bnd]) ,bdy)
      (define bnd+ (remove-complex-opera-exp* bnd))
      (define bdy+ (remove-complex-opera-exp* bdy))
      `(let ([,v ,bnd+]) ,bdy+)]
    ; operators
    [`(read) e]
    [`(+ ,e1 ,e2) (simplify-operator2 '+ e1 e2)]
    [`(- ,e) (simplify-operator1 '- e)]
    ))

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* p)
  (match p
    [`(program ,info ,e)
     `(program ,info ,(remove-complex-opera-exp* e))]
    ))

(define remove-complex-test
  `(+ (+ 1 2) (+ (+ 3 (read)) (+ (read) 4))))

(define remove-complex-expected
  '(let ((_0 (+ 1 2)))
     (let ((_1 (let ((_2 (let ((_3 (read)))
                           (+ 3 _3))))
                 (let ((_4 (let ((_5 (read)))
                             (+ _5 4))))
                   (+ _2 _4)))))
       (+ _0 _1))))

;;-------------------------------------------------------------------------------

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
