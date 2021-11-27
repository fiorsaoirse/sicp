#lang sicp

(#%require rackunit)

(define (make-interval a b)
    (cons a b)
)

(define (lower-bound interval)
    (car interval)
)

(define (upper-bound interval)
    (cdr interval)
)

(define test-interval (make-interval 1.0 1.2))

(check-equal? (lower-bound test-interval) 1.0)

(check-equal? (upper-bound test-interval) 1.2)

