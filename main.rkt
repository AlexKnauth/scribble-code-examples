#lang racket/base

(provide code-examples
         make-code-eval
         )

(require racket/list
         racket/sandbox
         racket/format
         racket/string
         scribble/manual
         (only-in scribble/decode pre-flow?)
         syntax/parse ; for run-time
         )

(define (make-code-eval #:lang lang)
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-propagate-exceptions #f]
                    [sandbox-propagate-breaks #f])
       (make-module-evaluator (string-append "#lang " lang "\n"))))))

;; example use of code-examples:
;; @code-examples[#:lang "at-exp racket" #:context #'here]|{
;; (+ 1 2)
;; @+[1 3]
;; }|
(define (code-examples #:lang lang-line-ish
                       #:context context
                       #:inset? [inset? #t]
                       #:lang-line? [lang-line? #f]
                       #:show-lang-line [show-lang-line #f]
                       #:eval [evaluator (make-code-eval #:lang lang-line-ish)]
                       . str-args)
  (define lang-line (string-append "#lang " lang-line-ish "\n"))
  (define full-str (apply string-append lang-line str-args))
  (define m (str-w/-lang->module-syntax full-str))
  (define-values [m-lang forms]
    (syntax-parse m #:datum-literals (module #%module-begin)
      [(module _ m-lang:expr (#%module-begin stuff ...))
       (values (syntax->datum #'m-lang) (syntax->list #'(stuff ...)))]))
  ;; zero-indexed end positions in the full-str string
  (define end-positions
    (for/list ([form (in-list forms)])
      ; syntax-positions are one-indexed, so use sub1
      (sub1 (+ (syntax-position form) (syntax-span form)))))
  (define strs
    (for/list ([start (in-list (cons (string-length lang-line) end-positions))]
               [end (in-list end-positions)])
      (string-trim (substring full-str start end) #:left? #true #:right? #false)))
  (define codes
    (for/list ([str (in-list strs)])
      (define code (codeblock0 #:keep-lang-line? #f #:context context (string-append lang-line str)))
      code))
  ;; resultss : (Listof (Listof Scribble-Stuff))
  (define resultss
    (for/list ([form (in-list forms)])
      (define results
        (call-with-values (λ () (evaluator form)) list))
      (define output (get-output evaluator))
      (define error-output (get-error-output evaluator))
      (append*
       (if (not (= (string-length output) 0))
           (list (racketoutput (literal output)))
           '())
       (if (not (= (string-length error-output) 0))
           (list (racketerror (literal error-output)))
           '())
       (for/list ([result (in-list results)])
         (if (not (void? result))
             (list (racketresultfont (~v result) #:decode? #f))
             '())))))
  (define interaction
    (above*
     (append*
      (for/list ([code (in-list codes)]
                 [results (in-list resultss)])
        (cons
         (beside/baseline (tt ">") code #:sep (hspace 1))
         results)))))
  (cond [(or show-lang-line lang-line?)
         (define lang-line-to-show
           (cond [(boolean? show-lang-line)
                  (codeblock0 #:context context (string-append "#lang " lang-line-ish))]
                 [(pre-flow? show-lang-line)
                  show-lang-line]
                 [else (error 'code-examples
                              "expected (or/c boolean? pre-flow?) for #:show-lang-line, given: ~v"
                              show-lang-line)]))
         (nested #:style (if inset? 'code-inset #f)
                 lang-line-to-show
                 interaction)]
        [else
         (nested #:style (if inset? 'code-inset #f)
                 interaction)]))

(define (above* stuff)
  (tabular
   (for/list ([stuff (in-list stuff)])
     (list stuff))))

(define (beside*/baseline #:sep sep stuff)
  (tabular
   #:cell-properties '((baseline))
   #:sep sep
   (list stuff)))

(define (beside/baseline #:sep sep . stuff)
  (beside*/baseline #:sep sep stuff))



(define (str-w/-lang->module-syntax #:src [src #f] . strs)
  (parameterize ([read-accept-lang #t]
                 [read-accept-reader #t]
                 [port-count-lines-enabled #t])
    (read-syntax
     (or src 'str-w/-lang->module-syntax)
     (open-input-string
      (apply string-append strs)))))

