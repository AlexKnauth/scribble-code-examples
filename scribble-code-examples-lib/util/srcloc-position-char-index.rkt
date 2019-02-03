#lang racket/base

(provide srcloc-position->char-index)

(module+ test
  (require rackunit racket/list syntax/srcloc))

;; srcloc-position->char-index : String -> [PosInt -> Natural]
;; Given a string `S` that represents a program `P`, produce a
;; function `f` such that for all syntax objects `stx` in `P`,
;; `(f (syntax-position stx))` returns the index into `S` that
;; corresponds to the first character of `stx`.
;; I wish this was as simple as `(const identity)`, but nope:
;; https://docs.racket-lang.org/reference/linecol.html
(define (srcloc-position->char-index s)
  ;; lst : [Listof [Pair PosInt Natural]]
  (define lst
    (parameterize ([port-count-lines-enabled #true])
      (define in (open-input-string s))
      (define-values [ln0 col0 pos0] (port-next-location in))
      (define idx0 0)
      (define chr0 (read-char in))
      (let loop ([lst (list (cons pos0 idx0))]
                 [pos pos0]
                 [idx idx0]
                 [chr chr0])
        (cond
          [(eof-object? chr)
           (close-input-port in)
           lst]
          [else
           (define-values [ln* col* pos*] (port-next-location in))
           (define idx* (add1 idx))
           (define chr* (read-char in))
           ;; INVARIANT:
           #; (cond [(char? chr*) (char=? (string-ref s idx*) chr*)]
                    [else         (= idx* (string-length s))])
           (define lst*
             (cond
               [(= (- pos* pos) (- idx* idx)) lst]
               [else                          (cons (cons pos* idx*) lst)]))
           (loop lst* pos* idx* chr*)]))))

  ;; pos->index : PosInt -> Natural
  (define (pos->index p)
    (for/first ([pos/idx (in-list lst)]
                #:when (<= (car pos/idx) p))
      (+ (cdr pos/idx) (- p (car pos/idx)))))
  pos->index)

(module+ test
  ;; sub-syntax-objects : Any -> [Listof Syntax]
  (define (sub-syntax-objects v)
    (cond [(syntax? v) (list v)]
          [(list? v) (append-map sub-syntax-objects v)]
          [(pair? v) (append (sub-syntax-objects (car v))
                             (sub-syntax-objects (cdr v)))]
          [(hash? v) (append-map sub-syntax-objects (hash-values v))]
          [(box? v) (sub-syntax-objects (unbox v))]
          [(vector? v) (append-map sub-syntax-objects (vector->list v))]
          [(prefab-struct-key v)
           (append-map sub-syntax-objects (vector->list (struct->vector v)))]
          [else '()]))
  
  (define-check (check-srcloc-position-char-index str)
    (parameterize ([port-count-lines-enabled #true])
      (define pos->index (srcloc-position->char-index str))
      (define stx (read-syntax #f (open-input-string str)))
      (let loop ([sub-stx stx])
        (define sub-str
          (substring str
                     (pos->index (source-location-position sub-stx))
                     (pos->index (source-location-end sub-stx))))
        (define sub-dat
          (read (open-input-string sub-str)))
        (check-equal? sub-dat (syntax->datum sub-stx))
        (for-each loop (sub-syntax-objects (syntax-e sub-stx))))))

  (check-srcloc-position-char-index "(a b c)")
  (check-srcloc-position-char-index "(a\nb\rc\r\nd\te)")
  (check-srcloc-position-char-index "(a\nb\rc\r\nd\te\r\nλx:τ.Γ⊢e≫e-⇒τf\tg)")
  )

