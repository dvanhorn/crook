#lang racket
(provide port->string/skips
         syntax-car
         syntax-cdr
         symbol>=?
         symbol<?)

;; Port [Listof (Pairof Nat Nat)] -> String
;; like port->string, but skips over given position intervals
;; intervals should be in ascending order
(define (port->string/skips in skips)
  (define (loop skips)
    (match skips
      ['() (port->string in)]
      [(cons (cons start stop) skips)
       (string-append
        (read-string (- start (file-position in)) in)
        (begin
          (file-position in stop)
          (loop skips)))]))
  (loop skips))

(module+ test
  (require rackunit)
  (define (ps s skips)
    (call-with-input-string s
      (λ (in) (port->string/skips in skips))))

  (check-equal? (ps "abcd" '()) "abcd")
  (check-equal? (ps "abcd" '((0 . 0))) "abcd")
  (check-equal? (ps "abcd" '((0 . 1))) "bcd")
  (check-equal? (ps "abcd" '((0 . 3))) "d")
  (check-equal? (ps "abcd" '((0 . 4))) "")
  (check-equal? (ps "abcd" '((0 . 1) (2 . 3))) "bd"))

(define (symbol>=? x y)
  (string>=? (symbol->string x)
             (symbol->string y)))

(define (symbol<? x y)
  (string<? (symbol->string x)
            (symbol->string y)))

(define (syntax-car s)
  (syntax-case s ()
    [(s1 . s2) #'s1]))

(define (syntax-cdr s)
  (syntax-case s ()
    [(s1 . s2) #'s2]))