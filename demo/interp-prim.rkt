#lang crook
{:= B C D E}

{:> B}
(provide interp-prim1)

{:> B}
(define (interp-prim1 p v)
  (match p
    ['add1 (add1 v)]
    ['sub1 (sub1 v)]
    {:> D}
    ['zero? (zero? v)]))
