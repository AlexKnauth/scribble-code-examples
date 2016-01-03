#lang scribble/manual

@(require (for-label scribble-code-examples
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
A scribble examples form that works for non-s-expression languages
}

