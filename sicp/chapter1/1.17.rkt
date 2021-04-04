#lang sicp

(#%require rackunit)

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? n)
    (= (remainder n 2) 0)
)

(define (fast-mult a b)
    (define (iter base multi acc)
        (cond ((= multi 0) acc)
              ((even? multi) (iter (double base) (halve multi) acc))
              (else (iter base (- multi 1) (+ base acc)))
        )
    )

    (iter a b 0)
)

(check-equal? (fast-mult 2 5) 10)
(check-equal? (fast-mult 3 5) 15)
(check-equal? (fast-mult 2 7) 14)