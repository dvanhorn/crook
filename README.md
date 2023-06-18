# Crook: a Language for Varying Racket Programs

Crook is a language for expressing variations over a Racket program.  It expresses variants as an order set of
changes to a previous program.

For each variant, a submodule of the program is created that captures the state of
the program at that point.

For example:

```racket
#lang crook
(provide f)
(define (f x)
  (:> A B) x
  (:> B)   (add1 x))
(module+ test
  (require rackunit)
  (check-equal? (f 5) 6))
```
This expresses two variants of a program.  The first, variant `A`, defines `f` to be the identity
function; second, variant `B` revises that definition to be the `add1` function.  The notation 
`(:> A B)` expresses that the subsequent form should be included in all variants greater than
or equal to `A`, but less than `B`, while `(:> B)` expresses that the subsequent form should be 
included in all variants greater than or equal to `B`.

There are two submodules generated by this program: `A` and `B`.  Both provide `f` and both have 
`test` submodules of their own that will test their variant of the program.  So to access the `B` 
variation of `f`, you would `(require (submod "." B))` within the DrRacket REPL.

The top-level `test` submodule collects the tests of all the submodules, so it tests every variation.
In this example, the `A` variant has a test failure.

You can recover an equivalent Racket program for each variant via the `source` submodule of each
variant.  For example `(require (submod "." B source))` in the DrRacket REPL, prints:

```racket
#lang racket
(provide f)
(define (f x)
  (add1 x))
(module+ test
  (require rackunit)
  (check-equal? (f 5) 6))
```

See `demo/` for a more substantial example of building an interpreter for a language that grows
to include more features over time.
