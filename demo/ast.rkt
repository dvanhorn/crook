#lang crook
(provide {:> A} Lit {:> B} Prim1 {:> C D} IfZero {:> D} If {:> E} Var {:> E} Let)

{:> A} ;; type Expr = (Lit Datum)
{:> B} ;;           | (Prim1 Op1)

{:> A} ;; type Datum = Integer
{:> D} ;;            | Boolean

{:> B} ;; type Op1 = 'add1 | 'sub1
{:> D} ;;          | 'zero?

{:> A}   (struct Lit (d) #:prefab)
{:> B}   (struct Prim1 (p e) #:prefab)
{:> C D} (struct IfZero (e1 e2 e3) #:prefab)
{:> D}   (struct If (e1 e2 e3) #:prefab)
{:> E}   (struct Var (x) #:prefab)
{:> E}   (struct Let (x e1 e2) #:prefab)
