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
  (define next (string->symbol (string-append "tmp." (number->string _next-unqiue))))
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

;; c-stmt-uncover-locals : CStmt -> [CVar]
(define (c-stmt-uncover-locals c-stmt)
  (match c-stmt
    [`(assign ,var ,_) `(,var)]
   ))

;; c-tail-uncover-locals : CTail -> [CVar]
(define (c-tail-uncover-locals c-tail)
  (match c-tail
    [`(seq ,stmt ,tail)
     (set-union
      (c-stmt-uncover-locals stmt)
      (c-tail-uncover-locals tail))]
    [_ `()]))

;; c-prog-uncover-locals : CProgram -> CProgram
(define (c-prog-uncover-locals c-prog)
  (match c-prog
    [`(program ,info ,c-blocks)
     (define locals (apply set-union (dict-map-vals c-blocks c-tail-uncover-locals)))
     (define info+ (cons (cons 'locals locals) info))
     `(program ,info+ ,c-blocks)]
    ))

(define (dict-map-vals dict proc)
  (dict-map dict (λ (key val) (proc val))))

;; explicate-control : RProgram -> CProgram
(define (explicate-control r-prog)
  (define c-prog
    (match r-prog
      [`(program ,info ,e)
       (explicate-control-top e 'start)
       `(program ,info ,(hash->list block-label-to-tail))]
      ))
  (c-prog-uncover-locals c-prog))

(define expl-test-trm1
  42)
(define expl-test-prog1 `(program () ,expl-test-trm1))
(define expl-test1 (explicate-control expl-test-prog1))

(define explicate-test-trm
  '(let ([y (let ([x1 20])
                 (let ([x2 22])
                      (+ x1 x2)))])
        y))
(define nested-lets-r `(program () ,explicate-test-trm))

;;-------------------------------------------------------------------------------

;; c-arg->px-arg : CArg -> PXArg
(define (c-arg->px-arg a)
  (match a
    [(? symbol?) `(var ,a)]
    [(? integer?) `(int ,a)]
    ))

;; px-arg-eq? : PXArg -> PXArg -> Bool
(define (px-arg-eq? a1 a2)
  (match* (a1 a2)
    [(`(reg ,r1) `(reg ,r2)) (eq? r1 r2)]
    [(_ _) (eq? a1 a2)]))

;; assign-read-instrs : PXArg -> [PXInstr]
(define (assign-read-instrs dst)
  `((callq read_int)
    (movq (reg rax) ,dst)))

;; assign-arg-instrs : PXArg -> [PXInstr]
(define (assign-arg-instrs src dst)
  `((movq ,src ,dst)))

;; assign-neg-instrs : PXArg -> [PXInstr]
(define (assign-neg-instrs arg dst)
  (if (px-arg-eq? arg dst)
      `((negq ,dst))
      `((movq ,arg ,dst)
        (negq ,dst))))

;; assign-add-instrs : PXArg -> [PXInstr]
(define (assign-add-instrs arg1 arg2 dst)
  (cond
    [(px-arg-eq? arg1 dst)
    `(addq ,arg2 ,dst)]
    [(px-arg-eq? arg2 dst)
    `(addq ,arg1 ,dst)]
    [else
     `((movq ,arg1 ,dst)
       (addq ,arg2 ,dst))]))

; eval-and-assign-instrs : PXArg -> CTerm -> [PInstr]
(define (eval-and-assign-instrs px-dst c-trm)
  (match c-trm
    ; args
    [(? symbol?)
     (assign-arg-instrs (c-arg->px-arg c-trm) px-dst)]
    [(? integer?)
     (assign-arg-instrs (c-arg->px-arg c-trm) px-dst)]
    ; operators
    [`(read)
     (assign-read-instrs px-dst)]
    [`(+ ,a1 ,a2)
     (assign-add-instrs (c-arg->px-arg a1) (c-arg->px-arg a2) px-dst)]
    [`(- ,a)
     (assign-neg-instrs (c-arg->px-arg a) px-dst)]
  ))

;; Convert the C statement into PX instructions.
;; c-stmt->px-instrs : CStmt -> [PXInstr]
(define (c-stmt->px-instrs stmt)
  (match stmt
    [`(assign ,var ,trm) (eval-and-assign-instrs (c-arg->px-arg var) trm)]
   ))

;; Convert the C tail into PX instructions that returns by jumping to the
;; given label.
;; c-tail->p-instrs : PXLabel -> CTail -> [PInstr]
(define (c-tail->px-instrs jump-on-ret c-tail)
  (match c-tail
    [`(seq ,stmt ,tail)
     (append
      (c-stmt->px-instrs stmt)
      (c-tail->px-instrs jump-on-ret tail))]
    [`(return ,trm)
     ;(eval-and-assign-instrs '(reg rax) trm)]
     (append
      (eval-and-assign-instrs '(reg rax) trm)
      `((jmp ,jump-on-ret)))]
   ))

;; c-blocks->p-blocks : [(CLabel, CTail)] -> [(PXLabel, PXTail)]
(define (c-blocks->px-blocks c-blocks)
  (map
   (λ (cb)
     (match cb
       [`(,label . ,tail)
        (define instrs (c-tail->px-instrs 'conclusion tail))
        (define block-info `())
        (define block (append `(block ,block-info) instrs))
        `(,label . ,block)]))
   c-blocks))

;; select-instructions : CProgram -> PXPorgram
(define (select-instructions c-prog)
  (match c-prog
    [`(program ,info ,c-blocks)
     `(program ,info ,(c-blocks->px-blocks c-blocks))]
    ))

;;-------------------------------------------------------------------------------

;; map-px-blocks : [(PXLabel, 'Block, PXBlockInfo [PXInstr])]
;;              -> ([PXInstr]
;;              -> [PXInstr])
;;              -> [(PXLabel, 'Block, PXBlockInfo, [PXInstr])]
(define (map-px-blocks blocks proc)
  (map (λ (b)
         (match b
           [`(,label block ,b-info ,instrs ...)
            (define instrs+ (proc instrs))
            (append `(,label block ,b-info) instrs+)]))
       blocks))

;; replace-var-in-arg : PXArg -> PXArg
(define (replace-var-in-arg var-to-arg arg)
  (match arg
    [`(var ,v) (dict-ref var-to-arg v)]
    [_ arg]))

;; replace-vars-in-instr : Map PXVar -> PXArg -> PXInstr -> PXInstr
(define ((replace-vars-in-instr var-to-arg) instr)
  (match instr
    [`(addq ,src ,dst)
     (define src+ (replace-var-in-arg var-to-arg src))
     (define dst+ (replace-var-in-arg var-to-arg dst))
     `(addq ,src+ ,dst+)]
    [`(negq ,dst)
     (define dst+ (replace-var-in-arg var-to-arg dst))
     `(negq ,dst+)]
    [`(movq ,src ,dst)
     (define src+ (replace-var-in-arg var-to-arg src))
     (define dst+ (replace-var-in-arg var-to-arg dst))
     `(movq ,src+ ,dst+)]
    [_ instr]
    ))

;; assign-var-homes : [PXVar] -> Map PXVar PXArg
(define (assign-var-homes vars)
  (for/list ([i (in-naturals)]
             [v vars])
    (define offset (* -8 (+ 1 i)))
    (define home `(deref rbp ,offset))
    (cons v home)))

;; stack-size : [PXVar] -> Int
(define (stack-size locals)
  (define num-locals (length locals))
  (cond
    [(= 0 num-locals)
     0]
    [(= 0 (modulo 2 num-locals))
     (* 8 num-locals)]
    [else
     (+ (* 8 num-locals) 8)]
    ))

;; assign-homes : PXProg -> PXProg
(define (assign-homes p)
  (match p
    [`(program ,info ,blocks)
     (define locals (dict-ref info 'locals))
     (define var-to-arg (assign-var-homes locals))
     (define blocks+ (map-px-blocks blocks (λ (instrs)
                                             (map (replace-vars-in-instr var-to-arg) instrs))))
     (define info+ (dict-set info 'stack-size (stack-size locals)))
     `(program ,info+ ,blocks+)]
    ))

;(define my-test2 (assign-homes my-test))

(define (run-compiler4 r)
  (select-instructions (explicate-control (remove-complex-opera* (uniquify `(program () ,r))))))
(define (run-compiler5 r)
  (assign-homes (select-instructions (explicate-control (remove-complex-opera* (uniquify `(program () ,r)))))))

;;-------------------------------------------------------------------------------

;; Return true iff PXIR arg is a dereference.
;; px-arg-is-deref? : PXArg -> Bool
(define (px-arg-is-deref? arg)
  (match arg
    [`(deref ,_ ,_) #t]
    [_ #f]
    ))         

;; If the given instruction is a movq with matching a source and destination
;; then remove the instruction.
;; patch-unneeded-movq : PXInstr -> [PXInstr]
(define (patch-unneeded-movq instr)
  (match instr
    [`(movq ,src ,dst)
     #:when (px-arg-eq? src dst)
     `()]
    [_
     `(,instr)]
    ))

;; If the given instruction is allowed to have at most one memory reference
;; argument then patch the instruction so that it adheres to this rule. 
;; 
;; In general if an instruction has a src and a dst argument that are both memory
;; references we patch the instruction by moving the src into the RAX register. We
;; then perform the original instruction using RAX as the src and the original dst
;; argument as the dst.
;; patch-double-ref : PXInstr -> [PXInstr]
(define (patch-double-ref instr)
  (match instr
    [`(movq ,src ,dst)
     #:when (and (px-arg-is-deref? src) (px-arg-is-deref? dst))
     `((movq ,src (reg rax))
       (movq (reg rax) ,dst))]
    [`(addq ,src ,dst)
     #:when (and (px-arg-is-deref? src) (px-arg-is-deref? dst))
     `((movq ,src (reg rax))
       (addq (reg rax) ,dst))]
    [_
     `(,instr)]
    ))

;; Apply the patch instruction to each instruction in the list of instructions.
;; apply-instr-patch :: (PXInstr -> [PXInstr]) -> [PXInstr] -> [PXInstr]
(define (apply-instr-patch proc instrs)
  (append* (map proc instrs)))

;; patch-instructions-in-block : [PXInstr] -> [PXInstr]
(define (patch-instructions-in-block instrs)
  (define instrs+ (apply-instr-patch patch-unneeded-movq instrs))
  (define instrs++ (apply-instr-patch patch-double-ref instrs))
  instrs++)

;; patch-instructions : psuedo-x86 -> x86
(define (patch-instructions p)
  (match p
    [`(program ,info ,blocks)
     (define blocks+ (map-px-blocks blocks patch-instructions-in-block))
     `(program ,info ,blocks+)]
    ))

;;-------------------------------------------------------------------------------

(define (mk-main-block stack-space jump-to)
  `(main
    block
    ()
    (pushq (reg rbp))
    (movq (reg rsp) (reg rbp))
    (subq (int ,stack-space) (reg rsp))
    (jmp ,jump-to)
    ))

(define (mk-conclusion-block stack-space)
  `(conclusion
    block
    ()
    (addq (int ,stack-space) (reg rsp))
    (popq (reg rbp))
    (retq)
    ))

(define (show-px-arg arg)
  (match arg
    [`(int ,n) (format "$~a" n)]
    [`(reg ,r) (format "%~a" r)]
    [`(deref ,r ,off) (format "~a(%~a)" off r)]
    [`(var ,v) (format "<~a>" v)]
    ))

(define (show-px-instr instr)
  (match instr
    [`(addq ,src ,dst)
     (format "addq ~a, ~a" (show-px-arg src) (show-px-arg dst))]
    [`(subq ,src ,dst)
     (format "subq ~a, ~a" (show-px-arg src) (show-px-arg dst))]
    [`(negq ,dst)
     (format "negq ~a" (show-px-arg dst))]
    [`(movq ,src ,dst)
     (format "movq ~a, ~a" (show-px-arg src) (show-px-arg dst))]
    [`(pushq ,src)
     (format "pushq ~a" (show-px-arg src))]
    [`(popq ,dst)
     (format "popq ~a" (show-px-arg dst))]
    [`(jmp ,label)
     (format "jmp ~a" label)]
    [`(callq ,label)
     (format "callq ~a" label)]
    [`(retq)
     "retq"]
    ))

(define (show-px-block b)
  (match b
    [`(,label block ,b-info ,instrs ...)
     (string-append*
      (format "~a:\n" label)
      (map (λ (i) (format "    ~a\n" (show-px-instr i)))
           instrs)
      )]))

(define (show-px-prog main-block conclusion-block prog-blocks)
  (define blocks-str (string-append* (map show-px-block prog-blocks)))
  (string-append
   blocks-str
   "\n"
   "    .global main\n"
   "\n"
   (show-px-block main-block)
   "\n"
   (show-px-block conclusion-block)
   ))
  
;; print-x86 : PXProg -> String
(define (print-x86 p)
  (match p
    [`(program ,info ,blocks)
     (define stack-size (dict-ref info 'stack-size))
     (define main-block (mk-main-block stack-size 'start))
     (define conclusion-block (mk-conclusion-block stack-size))
     (show-px-prog main-block conclusion-block blocks)]))

(define my-test (patch-instructions (assign-homes (select-instructions (explicate-control nested-lets-r)))))
