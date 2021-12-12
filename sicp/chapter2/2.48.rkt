#lang sicp

(#%require rackunit)

; ---- upper level of the abstraction
; ---- uses entities and operations from lower level 

(define (make-segment vector1 vector2)
    (cons vector1 vector2)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)
