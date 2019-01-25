#lang scribble/manual
@(require "../main.rkt"
          (for-label racket))

@code-examples[#:lang "at-exp racket" #:context #'here #:show-lang-line #t]|{
(+ 1 2)
@+[1 3]
(define @f[x]
  x)
@f{hello world}
'(1 2 3)
;; this prints it to the current output port
(print '(1 2 3))
;; but this prints it to the current error port
(eprintf "~v" '(1 2 3))
}|

