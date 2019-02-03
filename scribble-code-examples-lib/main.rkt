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
         "util/srcloc-position-char-index.rkt"
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
    (split-module-syntax m))

  (define strs
    (source-location-strs full-str (string-length lang-line) forms))

  (define interaction
    (above*
     (append*
      (for/list ([str (in-list strs)]
                 [form (in-list forms)])
        (evaluation-interaction str evaluator form
                                #:lang-line lang-line
                                #:context context)))))
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

;; ---------------------------------------------------------

;; evaluation-interaction :
;;   String Evaluator Stx #:lang String #:context Stx
;;   ->
;;   [Listof ScribbleStuff]
(define (evaluation-interaction str evaluator form
                                #:lang-line lang-line
                                #:context context)
  (define code
    (codeblock0 #:keep-lang-line? #f #:context context
                (string-append lang-line str)))
  (define results
    (evaluation-results evaluator form))
  (cons
   (beside/baseline (tt ">") code #:sep (hspace 1))
   results))

;; evaluation-results : Evaluator Stx -> [Listof Scribble-Stuff]
(define (evaluation-results evaluator form)
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
         '()))))

;; ---------------------------------------------------------

;; [Listof ScribbleStuff] -> ScribbleStuff
(define (above* stuff)
  (tabular
   (for/list ([stuff (in-list stuff)])
     (list stuff))))

;; #:sep ScribbleStuff [Listof ScribbleStuff] -> ScribbleStuff
(define (beside*/baseline #:sep sep stuff)
  (tabular
   #:cell-properties '((baseline))
   #:sep sep
   (list stuff)))

;; #:sep ScribbleStuff ScribbleStuff ... -> ScribbleStuff
(define (beside/baseline #:sep sep . stuff)
  (beside*/baseline #:sep sep stuff))

;; ---------------------------------------------------------

;; str-w/-lang->module-syntax : String ... [#:src Any] -> ModuleStx
(define (str-w/-lang->module-syntax #:src [src #f] . strs)
  (parameterize ([read-accept-lang #t]
                 [read-accept-reader #t]
                 [port-count-lines-enabled #t])
    (read-syntax
     (or src 'str-w/-lang->module-syntax)
     (open-input-string
      (apply string-append strs)))))

;; split-module-syntax : ModuleStx -> (values ModulePath [Listof Stx])
(define (split-module-syntax m)
  (syntax-parse m #:datum-literals (module #%module-begin)
    [(module _ m-lang:expr (#%module-begin stuff ...))
     (values (syntax->datum #'m-lang) (syntax->list #'(stuff ...)))]
    [(module _ m-lang:expr stuff ...)
     (values (syntax->datum #'m-lang) (syntax->list #'(stuff ...)))]))

;; source-location-strs : String Natural [Listof Stx] -> [Listof String]
(define (source-location-strs full-str first-start forms)
  ;; pos->index : PosInt -> Natural
  (define pos->index (srcloc-position->char-index full-str))

  ;; zero-indexed end positions in the full-str string
  (define end-positions
    (for/list ([form (in-list forms)])
      ; syntax-positions are different from string char-indexes,
      ; so convert using pos->index
      (pos->index (syntax-end-position form))))

  ;; zero-indexed start positions in the full-str string
  ;; Each form "starts" from the end-position of the
  ;; previous one so that comments written before a form are
  ;; included in that form
  (define start-positions
    (cons first-start (drop-right end-positions 1)))

  (for/list ([start (in-list start-positions)]
             [end (in-list end-positions)])
    (string-trim (substring full-str start end) #:left? #true #:right? #false)))

;; syntax-end-position : Syntax -> [Maybe PositiveInteger]
;; Produces the 1-indexed position of the end of the syntax object
(define (syntax-end-position stx)
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (and pos span (+ pos span)))

