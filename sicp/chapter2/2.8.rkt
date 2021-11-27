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

(define (sub-interval a b)
    (make-interval (- (lower-bound a) (upper-bound b))
                   (- (upper-bound a) (lower-bound b)))
)

(define first-interval (make-interval 2.0 2.5))

(define second-interval (make-interval 1.0 1.2))

(define result (sub-interval first-interval second-interval))

(check-equal? (lower-bound result) 0.8)

(check-equal? (upper-bound result) 1.5)