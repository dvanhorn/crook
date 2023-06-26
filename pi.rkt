#lang racket
(provide main)

;; This is a command line utility for projecting out a variant of
;; a crook program and saving it in a given destination directory.

;; The source directory should contain .rkt files that are all written in
;; crook.

;; e.g.
;; racket -t pi.rkt -m demo/ dupe/ D

(define (main src dst x)
  (parameterize ((current-directory src))
    (for ([p (in-directory #f)])
      (when (rkt-extension? p)
        (let-values ([(base name root?) (split-path p)])
          (define dir
            (match base
              ['relative dst]
              [else (build-path dst base)]))
          (define f (build-path dir name))
          (when (member (string->symbol x)
                        (dynamic-require (list 'submod
                                               (list 'file (path->string p))
                                               'configs)
                                         'configs))
            
            (unless (directory-exists? dir)
              (make-directory dir))
        
            (printf "creating ~a.\n" f)
            (with-output-to-file f
              (λ ()
                (dynamic-require (list 'submod
                                       (list 'file (path->string p))
                                       (string->symbol x)
                                       'source)
                                 #f))
              #:exists 'replace)))))))

;; Path -> Boolean
(define (rkt-extension? p)
  (match (path-get-extension p)
    [#".rkt" #t]
    [_ #f]))
  
