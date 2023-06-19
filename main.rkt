#lang racket

(module reader racket
  (require syntax/strip-context)

  (provide select dropped all select-require variants greatest<=?
           (rename-out [literal-read read]
                       [literal-read-syntax read-syntax]))

  (require (only-in scribble/comment-reader
                    read-syntax))

  ;; Symbol Syntax -> Syntax
  (define (select x s)
    (syntax-case s (:>)
      [((:> . c) s1 . s2)
       (if (holds? x (syntax->datum #'c))
           (with-syntax ([s1 (select x #'s1)]
                         [s2 (select x #'s2)])
             (syntax/loc s (s1 . s2)))
           (select x #'s2))]
      [(s1 . s2)
       (with-syntax ([s1 (select x #'s1)]
                     [s2 (select x #'s2)])
         (syntax/loc s (s1 . s2)))]
      [s #'s]))

  (define (syntax-car s)
    (syntax-case s ()
      [(s1 . s2) #'s1]))

  ;; Symbol Syntax -> [Listof Skip]
  ;; Like select, but returns a list of all the
  ;; syntax objects, including the specs, that are dropped

  (define (adj n) (- n 2)) ; -1 for starting "[", -1 for 0-based
  (define (dropped x s)
    (syntax-case s (:>)
      [((:> . c) s1 . s2)
       (syntax-case #'s2 ()
         [()
          (if (holds? x (syntax->datum #'c))
              (cons (cons (adj (syntax-position (syntax-car s)))
                          (- (syntax-position #'s1)
                             (syntax-position (syntax-car s))))
                    (dropped x #'s1))
              (begin
                (printf "HERE\n")
                (list (cons (adj (syntax-position (syntax-car s)))
                            (- (+ (syntax-position #'s1)
                                  (syntax-span #'s1))
                               (syntax-position (syntax-car s)))))))]
         [_
          (if (holds? x (syntax->datum #'c))
              (cons (cons (adj (syntax-position (syntax-car s)))
                          (- (syntax-position #'s1)
                             (syntax-position (syntax-car s))))
                    (append (dropped x #'s1)
                            (dropped x #'s2)))
              (cons (cons (adj (syntax-position (syntax-car s)))
                          (- (syntax-position (syntax-car #'s2))
                             (syntax-position (syntax-car s))))
                    (dropped x #'s2)))])]
      [(s1 . s2)
       (append (dropped x #'s1)
               (dropped x #'s2))]
      [_ '()]))

  ;; Syntax -> [Listof Symbol]
  ;; unique list of variants mentioned in s
  (define (all s)
    (define (all s)
      (syntax-case s (:>)
        [((:> x . _) s1 . s2)
         (cons (syntax->datum #'x)
               (append (all #'s1) (all #'s2)))]
        [(s1 . s2)
         (append (all #'s1) (all #'s2))]
        [s '()]))
    (set->list (apply seteq (all s))))

  (define (port->string/skips in skips)
    (define (loop skips)
      (match skips
        ['() (port->string in)]
        [(cons (cons pos span) skips)
         (string-append
          (read-string (- pos (file-position in)) in)
          (begin
            (file-position in (+ pos span))
            (loop skips)))]))
    (loop skips))

  (define (select-require x fs)
    (syntax-case fs ()
      [(f ...)
       (with-syntax ([(f ...) (map (λ (s) (select-req-file x s))
                                   (syntax->list #'(f ...)))])
         #'(f ...))]))

  (define (get-configs s)
    (dynamic-require (list 'submod (syntax->datum s) 'configs) 'configs))

  (define (select-req-file x f)
    (syntax-case* f (require) (λ (x y) (eq? (syntax->datum x) (syntax->datum y)))
      [(require m)
       (if (string? (syntax->datum #'m))
           (with-syntax ([x-stx (greatest<=? x (get-configs #'m))])
             #'(require (submod m x-stx)))
           #'(require m))]
      [x #'x]))

  (define (holds? x c)
    (match c
      [(list y)   (symbol>=? x y)]
      [(list y z) (and (symbol>=? x y)
                       (symbol<? x z))]))

  (define (greatest<=? x ys)
    (greatest<=?/acc x #f ys))

  (define (greatest<=?/acc x m ys)
    (match ys
      ['() m]
      [(cons y ys)
       (greatest<=?/acc x
                        (if m
                            (if (and (symbol>=? y m)
                                     (symbol>=? x y))
                                y
                                m)
                            (if (symbol>=? x y)
                                y
                                m))
                        ys)]))

  (define (symbol>=? x y)
    (string>=? (symbol->string x)
               (symbol->string y)))

  (define (symbol<? x y)
    (string<? (symbol->string x)
              (symbol->string y)))

  (define (literal-read in)
    (syntax->datum
     (literal-read-syntax #f in)))

  (define (read-syntax-all src in)
    (define content (port->string in))
    (values content
            (call-with-input-string
             ;; wrap in parens and do single read-syntax
             (string-append "[" content "]")
             (λ (in)
               (read-syntax src in)
               #;(let loop ([stxs '()])
                 (let ((s (read-syntax src in)))
                   (if (eof-object? s)
                       (reverse stxs)
                       (loop (cons s stxs)))))))))

  (define (literal-read-syntax src in)
    (let-values ([(orig stxs) (read-syntax-all src in)])
      (variants stxs orig)))

  ;; Symbol Syntax String -> String
  ;; Pretty print variant of s as a string
  (define (syntax-format x s orig)
    (current-width 60)
    (program-format
     (string-append "#lang racket"
                    (call-with-input-string orig
                                            (λ (in)
                                              (port->string/skips
                                               in
                                               (dropped x s)))))))

  (require fmt racket/syntax)

  (define (variants stxs orig)
    (with-syntax ([stxs stxs])
      (match (all #'stxs)
        ['() (strip-context
              #'(module anything racket . stxs))]
        [ms
         (with-syntax
             ([mods (map (λ (m)
                           (with-syntax*
                               ([b (select m #'stxs)]
                                [src (syntax-format m #'stxs orig)]
                                [orig orig]
                                [c (select-require m #'b)]
                                [m m]
                                #;[src-debug (syntax-format #'c)])
                             #'(begin
                                 (module+ test
                                   (require (submod ".." m test)))
                                 (module m racket
                                   (module source racket
                                     (displayln src))
                                   (module original racket
                                     (displayln orig))
                                   #; (module source-debug racket
                                        (displayln src-debug))
                                   (module+ test)
                                   (define-syntax-rule (code:comment . _)
                                     (void))
                                   . c))))
                         ms)]
              [ms ms])
           (strip-context
            #'(module anything racket
                (module configs racket
                  (provide configs)
                  (define configs 'ms))
                . mods)))]))))
