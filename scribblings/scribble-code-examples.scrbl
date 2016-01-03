#lang scribble/manual

@(require scribble-code-examples
          (for-label scribble-code-examples
                     racket/base
                     racket/contract/base
                     scribble/decode
                     scribble/core
                     ))

@title{scribble-code-examples}

@defmodule[scribble-code-examples]

@defproc[(code-examples [#:lang language string?]
                        [#:context context syntax?]
                        [#:inset? inset? boolean? #true]
                        [#:show-lang-line show-lang-line (or/c boolean? pre-flow?) #false]
                        [#:eval evaluator evaluator? (make-code-eval #:lang language)]
                        [examples string?]
                        ...)
         block?]{
A scribble examples form that works for non-s-expression languages.

For example, this:
@verbatim|{
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(+ 1 2)
@+[1 3]
}|
}|
Produces this:
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(+ 1 2)
@+[1 3]
}|

And this:
@verbatim|{
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(require scribble-code-examples (for-label racket/base))
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(+ 1 2)
@+[1 3]
}|
}|
}|
Produces this:
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(require scribble-code-examples (for-label racket/base))
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(+ 1 2)
@+[1 3]
}|
}|
}
