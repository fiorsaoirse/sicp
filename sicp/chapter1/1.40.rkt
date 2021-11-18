#lang sicp

(#%require rackunit)

(define (cubic a b c) 
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

(check-equal? ((cubic 1 2 3) 4) 91)