#lang scribble/manual
@(require "../main.rkt"
          (for-label racket))

@code-examples[#:lang "at-exp racket" #:context #'here]|{
(+ 1 2)
@+[1 3]
(define @f[x]
  x)
@f{hello world}
'(1 2 3)
(print '(1 2 3))
(eprintf "~v" '(1 2 3))
}|

