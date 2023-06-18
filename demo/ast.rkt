#lang crook
(provide {:> A}
         Lit
         {:> B}
         Prim1
         {:> C C}
         IfZero
         {:> D}
         If
         {:> E}
         Var
         {:> E}
         Let)

{:> A}
(struct Lit (d) #:prefab)
{:> B}
(struct Prim1 (p e) #:prefab)
{:> C C}
(struct IfZero (e1 e2 e3) #:prefab)
{:> D}
(struct If (e1 e2 e3) #:prefab)
{:> E}
(struct Var (x) #:prefab)
{:> E}
(struct Let (x e1 e2) #:prefab)

 