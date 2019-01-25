# scribble-code-examples
A scribble examples form that works for non-s-expression languages

Documentation: https://docs.racket-lang.org/scribble-code-examples/index.html

This repository contains two packages:
 - `scribble-code-examples-lib`, contains the `scribble-code-examples` module but no docs or tests
 - `scribble-code-examples`, contains the documentation and tests

```racket
(require scribble-code-examples)
```

In a Scribble Documentation file, writing this:
```racket
@code-examples[#:lang "at-exp racket" #:context #'here]|{
(+ 1 2)
@+[1 3]
}|
```

Produces output that shows a Repl interaction like this:
```racket
> (+ 1 2)
3
> @+[1 3]
4
```
