#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (sum_of_squares a b) (+ (square a) (square b)))

(define (max a b) ( if (> a b) a b) )

(define (min a b) ( if (< a b) a b) )

(define (sum_of_max a b c) (sum_of_squares (max a b) (max c (min a b))))

(check-equal? (square 2) 4)
(check-equal? (square 3) 9)

(check-equal? (sum_of_squares 2 3) 13)
(check-equal? (sum_of_squares 5 10) 125)

(check-equal? (sum_of_max 1 2 3) 13)
(check-equal? (sum_of_max 5 2 10) 125)