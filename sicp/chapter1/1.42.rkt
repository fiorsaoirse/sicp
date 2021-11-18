#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)

(check-equal? ((compose square inc) 6) 49)
(check-equal? ((compose square dec) 6) 25)
(check-equal? ((compose cube dec) 3) 8)