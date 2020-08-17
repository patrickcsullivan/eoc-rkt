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

;; CLabel = Symbol

;; _next-unqiue-block-num : Integer
(define _next-unqiue-block-num 0)

;; next-unique-block-label :: CLabel
(define (next-unique-block-label)
  (define next (string->symbol (string-append "_" (number->string _next-unqiue-block-num))))
  (set! _next-unqiue-block-num (+ _next-unqiue-block-num 1))
  next)

;; maps C block labels to C-tails 
;; block-label-to-tail : HashMap CLabel CTail
(define block-label-to-tail (make-hash))

;; Insert the block label and tail into the mapping of labels to tails.
;; insert-block! : CLabel -> CTail -> ()
(define (insert-block! label tail)
  (hash-set! block-label-to-tail label tail))

;; Convert an R operator argument to a C term. Only works for S val
;; terms and var terms since the uniquify pass should have converted all
;; operator args into val or var terms.
;; (This isn't terribly useful now but will be if I convert to typed Racket.)
;; r-prim-term->c-term :: RTerm -> CArg
(define (r-prim-term->c-term trm)
  (match trm
    [(? symbol?) trm]
    [(? integer?) trm]
    [_ (error "r-prim-term->c-term: input must be a val or var R-term")]))
  
;; Explicate the "tail" part of an R term. The "tail" of an R term is
;; the part of the term that should be executed last.
;; explicate-control-tail : RTerm -> (CTerm -> CTail) -> CTail
(define (explicate-control-tail r-trm do-after)
  (match r-trm
    ; primitives and vars
    [(? symbol?) (do-after (r-prim-term->c-term r-trm))]
    [(? integer?) (do-after (r-prim-term->c-term r-trm))]
    ; operators
    [`(read) (do-after `(read))]
    [`(+ ,t1 ,t2) (do-after `(+ ,(r-prim-term->c-term t1) ,(r-prim-term->c-term t2)))]
    [`(- ,t) (do-after `(- ,(r-prim-term->c-term t)))]
    ; language constructs
    [`(let ([,v ,bnd]) ,bdy)
      (define do-after-bnd (explicate-control-tail bdy do-after))
      (explicate-control-assign bnd v do-after-bnd)]
    ))

;; Explicate the binding term of a let term.
;; explicate-control-assign : RTerm -> CVar -> CTail -> CTail
(define (explicate-control-assign r-bnd assign-to tail)
  (match r-bnd
    ; primitives and vars
    [(? symbol?) `(seq (assign ,assign-to ,r-bnd) ,tail)]
    [(? integer?) `(seq (assign ,assign-to ,r-bnd) ,tail)]
    ; operators
    [`(read) `(seq (assign ,assign-to (read)) ,tail)]
    [`(+ ,t1 ,t2)
      (define assigned `(+ ,(r-prim-term->c-term t1) ,(r-prim-term->c-term t2)))
      `(seq (assign ,assign-to ,assigned) ,tail)]
    [`(- ,t)
      (define assigned `(- ,(r-prim-term->c-term t)))
      `(seq (assign ,assign-to ,assigned) ,tail)]
    ; language constructs
    [`(let ([,v ,bnd]) ,bdy)
      (define bdy-tail
        (explicate-control-tail bdy
                                (λ (ctrm) `(seq (assign ,assign-to ,ctrm) ,tail))))
      (explicate-control-assign bnd v bdy-tail)]
    ))

;; explicate-control-top : RTerm -> CLabel -> CTail
(define (explicate-control-top trm label)
  (define tail (explicate-control-tail trm (λ (ctrm) `(return ,ctrm))))
  (insert-block! label tail)
  tail)

;; explicate-control : RProgram -> CProgram
(define (explicate-control r-prog)
  (match r-prog
    [`(program ,info ,e)
     (explicate-control-top e 'start)
     `(program ,info ,(hash->list block-label-to-tail))]
    ))

(define expl-test-trm1
  42)
(define expl-test-prog1 `(program () ,expl-test-trm1))
(define expl-test1 (explicate-control expl-test-prog1))

(define explicate-test-trm
  '(let ([y (let ([x1 20])
                 (let ([x2 22])
                      (+ x1 x2)))])
        y))
(define explicate-test-prog `(program () ,explicate-test-trm))

;;-------------------------------------------------------------------------------

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions p)
  p)

;;-------------------------------------------------------------------------------

;; assign-homes : pseudo-x86 -> pseudo-x86
(define (assign-homes p)
  p)

;;-------------------------------------------------------------------------------

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  p)

;;-------------------------------------------------------------------------------
;; print-x86 : x86 -> string
(define (print-x86 p)
  "todo - implement print-x86")
