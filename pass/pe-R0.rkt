#lang racket

(require racket/fixnum)

;; Next we have the partial evaluation pass described in the book.
(define (pe-neg r)
  (define r (pe-exp r))
  (cond 
    [(fixnum? r) 
      (fx- 0 r)]
	  [else 
      `(- ,r)]))

(define (pe-add r1 r2)
  (cond 
    [(and (fixnum? r1) (fixnum? r2)) 
      (fx+ r1 r2)]
    [else 
      `(+ ,r1 ,r2)]))

(define (pe-exp e)
  (match e
    [(? fixnum?) 
      e]
    [`(read) 
      `(read)]
    [`(- ,e1) 
      (pe-neg (pe-exp e1))]
    [`(+ ,e1 ,e2) 
      (pe-add (pe-exp e1) (pe-exp e2))]
    ))

(define (pe-R0 p)
  (match p
    [`(program ,e) `(program ,(pe-exp e))]
    ))

(define my-test `(program (+ 1 (+ (read) 1))))