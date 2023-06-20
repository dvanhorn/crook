#lang crook
{:= A B C D E}
(provide main)

(require "parse.rkt")
(require "interp.rkt")

(define (f x)
  2
  {:> A C}
  1
  {:> C}
  2)

;; -> Void
;; Parse and interpret contents of stdin,
;; print result on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (println (interp (parse (read)))))
