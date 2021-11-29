#lang sicp

(#%require rackunit)

(define nil '())

(define (square x) (* x x))

(define (square-list items)
    (if (null? items)
        nil
        (cons (square (car items)) (square-list (cdr items)))
    )
)

(define (square-list-map items)
    (map (lambda (x) (square x)) items)
)

(check-equal? (square-list (list 1 2 3 4)) (list 1 4 9 16))

(check-equal? (square-list-map (list 1 2 3 4)) (list 1 4 9 16))