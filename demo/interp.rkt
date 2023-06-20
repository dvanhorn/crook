#lang crook
{:= A B C D E}

(provide interp)
(require "ast.rkt")
{:> B}
(require "interp-prim.rkt")

{:> A}   ;; type Value = Integer
{:> D}   ;;            | Boolean

{:> A E} ;; Expr -> Value

{:> E}   ;; type Env = [Listof (List Id Value)]

{:> E}   ;; Expr Env -> Value
(define (interp e)
  {:> A E}
  (match e
    {:> A}
    [(Lit d) d]
    {:> B}
    [(Prim1 p e) (interp-prim1 p (interp e))]
    {:> C D}
    [(IfZero e1 e2 e3)
     (if (zero? (interp e1))
         (interp e2)
         (interp e3))]
    {:> D}
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))])
  {:> E}
  (interp-env e '()))

{:> E} ;; Expr Env -> Value
{:> E}
(define (interp-env e r)
  (match e
    [(Lit d) d]
    [(Prim1 p e) (interp-prim1 p (interp-env e r))]    
    [(If e1 e2 e3)
     (if (interp-env e1 r)
         (interp-env e2 r)
         (interp-env e3 r))]
    [(Var x) (lookup r x)]
    [(Let x e1 e2)
     (let ((v (interp-env e1 r)))
       (interp-env e2 (cons (cons x v) r)))]))

{:> E} ;; Env Id -> Value
{:> E}
(define (lookup r x)
  (cdr (assq x r)))

(module+ test
  (require rackunit)
  {:> A}   (check-equal? (interp (Lit 1)) 1)
  {:> B}   (check-equal? (interp (Prim1 'add1 (Lit 1))) 2)
  {:> B}   (check-equal? (interp (Prim1 'sub1 (Lit 1))) 0)
  {:> C D} (check-equal? (interp (IfZero (Lit 0) (Lit 1) (Lit 2))) 1)
  {:> C D} (check-equal? (interp (IfZero (Lit 1) (Lit 1) (Lit 2))) 2)
  {:> D}   (check-equal? (interp (If (Prim1 'zero? (Lit 0)) (Lit 1) (Lit 2))) 1)
  {:> D}   (check-equal? (interp (If (Prim1 'zero? (Lit 1)) (Lit 1) (Lit 2))) 2)
  {:> E}   (check-equal? (interp (Let 'x (Lit 1) (Var 'x))) 1))
