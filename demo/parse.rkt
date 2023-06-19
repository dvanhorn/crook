#lang crook
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s {:> D} x)
  (match s    
    {:> A}
    [(? datum?) (Lit s)]
    {:> B}
    [(list (? op1? o) e) (Prim1 o (parse e))]
    {:> C D}
    [(list 'if (list 'zero? e1) e2 e3)
     (IfZero (parse e1) (parse e2) (parse e3))]
    {:> D}
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]    
    [_ (error "Parse error")]))

;; Any -> Boolean
(define (datum? x)
  {:> A D}
  (exact-integer? x)
  {:> D}
  (or (exact-integer? x)
      (boolean? x)
      {:> D} (char? x)))

;; Any -> Boolean
(define (op1? x)
  (memq x '(add1 sub1 {:> D} zero?)))
