#lang racket/base

(provide code-examples)

(require racket/list
         racket/sandbox
         racket/format
         scribble/manual
         syntax/parse ; for run-time
         )

;; example use of code-examples:
;; @code-examples[#:lang "at-exp racket" #:context #'here]|{
;; (+ 1 2)
;; @+[1 3]
;; }|
(define (code-examples #:lang lang-line-ish #:context context #:inset? [inset? #t] . str-args)
  (define lang-line (string-append "#lang " lang-line-ish "\n"))
  (define full-str (apply string-append lang-line str-args))
  (define m (str-w/-lang->module-syntax full-str))
  (define-values [m-lang forms]
    (syntax-parse m #:datum-literals (module #%module-begin)
      [(module _ m-lang:expr (#%module-begin stuff ...))
       (values (syntax->datum #'m-lang) (syntax->list #'(stuff ...)))]))
  (define strs
    (for/list ([form (in-list forms)])
      (define pos (syntax-position form))
      (define end (+ pos (syntax-span form)))
      (substring full-str (sub1 pos) (sub1 end))))
  (define evaluator
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-exceptions #f])
      (make-module-evaluator lang-line)))
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
  (cond
    [inset?
     (nested #:style 'code-inset interaction)]
    [else interaction]))

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

