#lang sicp

(#%require rackunit)

(define (first-denomination values)
    (car values)
)

(define (exept-first-denomination values)
    (cdr values)
)

(define (no-more? values)
    (null? values)
)

(define us-coins (list 50 25 10 5 1))

(check-equal? (first-denomination us-coins) 50)

(check-equal? (exept-first-denomination us-coins) (list 25 10 5 1))

(check-equal? (no-more? us-coins) #f)

(check-equal? (no-more? (list)) #t)