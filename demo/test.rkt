#lang crook
{:= A B C D E}

(require rackunit)
(require "parse.rkt")
(require "interp.rkt")

{:> A} (check-equal? (interp (parse 1)) 1)
{:> B} (check-equal? (interp (parse '(add1 1))) 2)
{:> C} (check-equal? (interp (parse '(if (zero? 0) 1 2))) 1)

