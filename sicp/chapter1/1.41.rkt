#lang sicp

(#%require rackunit)

(define (double f)
    (lambda (x) (f (f x)))
)

(check-equal? ((double inc) 1) 3)

(((double (double double)) inc) 5) 