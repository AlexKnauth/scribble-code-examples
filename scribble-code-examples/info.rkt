#lang info

(define collection "scribble-code-examples")

(define deps
  '("base"
    "scribble-code-examples-lib"
    "scribble-lib"
    "sandbox-lib"
    ))

(define build-deps '("racket-doc" "scribble-doc"))

(define scribblings '(["scribblings/scribble-code-examples.scrbl" () ("Scribble Libraries")]))

(define implies
  '("scribble-code-examples-lib"
    ))

(define update-implies
  '("scribble-code-examples-lib"
    ))

