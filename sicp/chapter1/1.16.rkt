#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (even? n)
    (= (remainder n 2) 0)
)

(define (fast-expt b n)
    (define (iter base pow acc)
        (cond ((= pow 0) acc)
              ((even? pow) (iter (square base) (/ pow 2) acc))
              (else (iter base (- pow 1) (* base acc)))
        )
    )

    (iter b n 1)
)

(check-equal? (fast-expt 2 5) 32)
(check-equal? (fast-expt 3 5) 243)
(check-equal? (fast-expt 2 7) 128)