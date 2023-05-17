#lang racket
(require racket/trace)

; Reflexive Closure

(define (Reflexive-Closure L S [RE (rSetBuilder S)] [NL '()] [k 0])
  (if (= k (length L))
      (append RE NL)
      (if (member? (list-ref L k) RE)
          (Reflexive-Closure L S RE NL (+ 1 k))
          (Reflexive-Closure L S RE (append (list (list-ref L k)) NL) (+ 1 k))
          )))

; Symmetric Closure

(define (Symmetric-Closure L)
  (sSetBuilder L))

; Transitive Closure

(define (Transitive-Closure L [TE (tSetBuilder L)] [NL '()] [k 0])
  (if (= k (length L))
      (rmvDup (append TE NL))
      (if (member? (list-ref L k) TE)
          (Transitive-Closure L TE NL (+ 1 k))
          (Transitive-Closure L TE (append (list (list-ref L k)) NL) (+ 1 k))
          )))

(trace Transitive-Closure)

; Helper Functions


; - Set Builders

(define (rSetBuilder L [NL'()])
  (if (null? L)
      NL
      (rSetBuilder (cdr L) (append (list (list (car L) (car L))) NL))))

(define (sSetBuilder L [NL '()])
  (if (null? L)
      NL
      (if (member? (car L) NL)
          (sSetBuilder (cdr L) NL)
          (if (equal? (car (car L)) (car (cdr (car L))))
              (sSetBuilder (cdr L) (append (list (car L)) NL))
               (sSetBuilder (cdr L) (append (list (car L) (reverse (car L))) NL))
          ))))

(define (tSetBuilder L [NL '()] [k 0])
  (if (= k (length L))
      NL
      (if (member? (car L) NL)
          (tSetBuilder L NL (+ 1 k))
          (tSetBuilder L (append (trans1 (list-ref L k) L) NL) (+ 1 k))
          )))

(trace tSetBuilder)

; - Other Helpers

(define (member? x L)
  (if (null? L)
      #f
      (if (equal? (car L) x)
          #t
          (or (member x (cdr L))))))

(define (trans1 P L [NL '()] [x (car P)] [y (car (cdr P))])
  (if (null? L)
      NL
      (if (and (equal? (car (car L)) y) (not (member? (append (list x) (car (cdr (car L)))) NL)))
          (trans1 P (cdr L) (append (list (append (list x) (list (car (cdr (car L)))))) NL))
          (trans1 P (cdr L) NL)
          )))

(define (rmvDup L [NL L])
  (if (null? L)
      NL
      (if (member? (car L) (remove (car L) NL))
          (rmvDup (cdr L) (remove (car L) NL))
          (rmvDup (cdr L) NL)
          )))
