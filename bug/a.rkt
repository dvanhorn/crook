#lang crook
{:= A B}
;; Comment causes this to be collapsed
(define x 100)
{:> B} ; comment

{:> A} ; ^ should have a blank line above


;; But an expression is OK
(define y 100)
{:> B} 'ok

{:> A} ; ^ has a blank line above

