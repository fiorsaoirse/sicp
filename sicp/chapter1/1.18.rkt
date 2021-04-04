#lang sicp

(#%require rackunit)

(define (double x) (+ x x))

(define (halve x) (truncate (/ x 2)))

(define (even? n)
    (= (remainder n 2) 0)
)

(define (fast a b)
    (define (iter a b acc)
        (cond ((= a 0) acc)
              ((even? a) (iter (halve a) (double b) acc))
              (else (iter (halve a) (double b) (+ acc b)))
        )
    )

    (iter a b 0)
)

(check-equal? (fast 13 19) 247)
(check-equal? (fast 10 13) 130)
(check-equal? (fast 2 15) 30)