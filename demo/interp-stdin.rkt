#lang crook
(:> A A)
(provide main)
(:> B B)
(provide main)
(:> C C)
(provide main)
(:> D D)
(provide main)
(:> D1 D1)
(provide main)
(:> E E)
(provide main)

(require "parse.rkt")
(require "interp.rkt")

;; -> Void
;; Parse and interpret contents of stdin,
;; print result on stdout
(define (main)
  (read-line) ; ignore #lang racket line
  (println (interp (parse (read)))))