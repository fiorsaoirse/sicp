#lang sicp

(#%require rackunit)

(define nil '())

(define (same-parity num . args)
    (define is-even (even? num))

    (define (accept? val)
        (equal? (even? val) is-even)
    )

    (define (inner source)
        (cond ((null? source) nil)
              ((accept? (car source)) (cons (car source) (inner (cdr source))))
              (else (inner (cdr source)))
        )
    )

    (cons num (inner args))
)

(check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))

(check-equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))