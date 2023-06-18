#lang racket

(module reader racket
  (require syntax/strip-context)
 
  (provide select all select-require variants greatest<=?
           (rename-out [literal-read read]
                       [literal-read-syntax read-syntax]))

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
    (let loop ([stxs '()])
      (let ((s (read-syntax src in)))
        (if (eof-object? s)
            (reverse stxs)
            (loop (cons s stxs))))))
  
  (define (literal-read-syntax src in)
    (variants (read-syntax-all src in)))

  ;; Syntax -> String
  ;; Pretty print version of s as a string
  (define (syntax-format s)           
    (program-format
     (format "#lang racket\n~a"
             (apply string-append (map (λ (f) (format "~s\n" f))
                                       (syntax->datum s))))))
  
  (require fmt racket/syntax)
  
  (define (variants stxs)
    (with-syntax ([stxs stxs])
      (match (all #'stxs)
        ['() (strip-context
              #'(module anything racket . stxs))]
        [ms
         (with-syntax
             ([mods (map (λ (m)
                           (with-syntax*
                               ([b (select m #'stxs)]
                                [c (select-require m #'b)]
                                [m m]
                                [src (syntax-format #'b)]
                                [src-debug (syntax-format #'c)])
                             #'(begin
                                 (module+ test
                                   (require (submod ".." m test)))
                                 (module m racket
                                   (module source racket
                                     (displayln src))
                                   (module source-debug racket
                                     (displayln src-debug))
                                   (module+ test)
                                   . c))))
                         ms)]
              [ms ms])
           (strip-context
            #'(module anything racket
                (module configs racket
                  (provide configs)
                  (define configs 'ms))
                . mods)))]))))
