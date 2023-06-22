#lang racket
(provide main)

;; This is a command line utility for projecting out a variant of
;; a crook program and saving it in a given destination directory.

;; The source directory should contain .rkt files that are all written in
;; crook.

;; e.g.
;; racket -t pi.rkt -m demo/ dupe/ D

(define (main src dst x)
  (define fs (filter rkt-extension? (directory-list src)))
  (unless (directory-exists? dst)
    (make-directory dst))

  (for ([f fs])
    (when (member (string->symbol x) (dynamic-require (list 'submod
                                                            (list 'file (path->string (build-path src f)))
                                                            'configs)
                                                      'configs))
      (printf "creating ~a.\n" f)
      (with-output-to-file (build-path dst f)
        (λ ()
          (dynamic-require (list 'submod
                                 (list 'file (path->string (build-path src f)))
                                 (string->symbol x)
                                 'source)
                           #f))
        #:exists 'replace))))

;; Path -> Boolean
(define (rkt-extension? p)
  (match (path-get-extension p)
    [#".rkt" #t]
    [_ #f]))
  
