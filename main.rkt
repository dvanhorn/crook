#lang racket

(module reader racket
  (require syntax/strip-context
           "private/utils.rkt")

  (provide select dropped all select-require variants #;greatest<=?
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


  ;; Symbol Syntax -> [Listof Skip]
  ;; Like select, but produces a skip list for removing
  ;; syntax that was not selected
  (define (dropped x s)
    (define (dropped s o)
      (syntax-case s (:> :=)
        ;[((:= . _) . s) (dropped #'s o)]
        [((:> . c) s1 . s2)
         (if (holds? x (syntax->datum #'c))
             (cons (cons (syntax-position (syntax-car s))
                         (syntax-position #'s1))
                   (append (dropped #'s1 o)
                           (dropped #'s2 (+ (syntax-position #'s1)
                                            (syntax-span #'s1)))))
             (cons (cons o (+ (syntax-position #'s1)
                              (syntax-span #'s1)))
                   (dropped #'s2 (+ (syntax-position #'s1)
                                    (syntax-span #'s1)))))]
        [(s1 . s2)
         (append (dropped #'s1 o)
                 (dropped #'s2 (+ (syntax-position #'s1)
                                  (syntax-span #'s1))))]
        [_ '()]))
    (map (λ (s) (cons (- (car s) 2) (- (cdr s) 2)))
         (dropped s 0)))

  ;; Syntax -> [Listof Symbol]
  ;; unique list of variants mentioned in s
  (define (all s)
    (syntax-case s (:=)
      [((:= . vs) . _)
       (syntax->datum #'vs)]))
  
  #;
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

  (define (select-require x fs)
    (syntax-case fs ()
      [(f ...)
       (with-syntax ([(f ...) (map (λ (s) (select-req-file x s))
                                   (syntax->list #'(f ...)))])
         (syntax/loc fs (f ...)))]))

  #;
  (define (get-configs s)
    (dynamic-require (list 'submod (syntax->datum s) 'configs) 'configs))

  (define (select-req-file x f)
    (syntax-case* f (require provide all-from-out)
        (λ (x y) (eq? (syntax->datum x) (syntax->datum y)))
      [(require m)
       (if (string? (syntax->datum #'m))
           (with-syntax ([x x])
             (syntax/loc #'f (require (submod m x))))
           (syntax/loc #'m (require m)))]
      [(provide (all-from-out m))
       (if (string? (syntax->datum #'m))
           (with-syntax ([x x])
             (syntax/loc #'f (provide (all-from-out (submod m x)))))
           (syntax/loc #'m (require m)))]
      [x #'x]))

  (define (holds? x c)
    (match c
      [(list y)   (symbol>=? x y)]
      [(list y z) (and (symbol>=? x y)
                       (symbol<? x z))]))

  #;
  (define (greatest<=? x ys)
    (greatest<=?/acc x #f ys))
  #;
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
               (read-syntax src in)))))

  (define (literal-read-syntax src in)
    (let-values ([(orig stxs) (read-syntax-all src in)])
      (variants stxs orig)))

  ;; Symbol Syntax String -> Bytes
  ;; Print variant of s as a string
  ;; Given both syntax and textual representation of program
  (define (syntax-format x s orig)
    (bytes-append #"#lang racket"
                  (call-with-input-string orig
                                          (λ (in)
                                            (port->bytes/skips
                                             in
                                             (cons (cons 1 (+ 2 (syntax-span (syntax-car s))))
                                                   (dropped x (syntax-cdr s))))))))

  (require racket/syntax)

  (define (variants stxs orig)
    (with-syntax ([stxs stxs])
      (match (all #'stxs)
        ['() (strip-context
              #'(module anything racket . stxs))]
        [ms
         (with-syntax
             ([mods (map (λ (m)
                           (with-syntax*
                               ([b (select m (syntax-cdr #'stxs))]
                                [src (syntax-format m #'stxs orig)]
                                [orig (string-append "#lang crook" orig)]
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
                                   (module comment racket
                                     (provide code:comment)
                                     (define-syntax-rule (code:comment . _)
                                       (void)))
                                   #; (module source-debug racket
                                        (displayln src-debug))
                                   (module+ test)
                                   (require (submod "." comment))
                                   . c))))
                         ms)]
              [ms ms])
           (strip-context
            #'(module anything racket
                (module configs racket
                  (provide configs)
                  (define configs 'ms))
                . mods)))]))))
