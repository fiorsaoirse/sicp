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

(define (make-center-percent center percent)
    (let (
            (lower (- center (* center (/ percent 100))))
            (upper (+ center (* center (/ percent 100))))
         )
         (make-interval lower upper)
    )
)

(define (center interval)
    (/ (+ (lower-bound interval) (upper-bound interval)) 2)
)

(define (percent interval)
    (let (
            (lower (lower-bound interval))
            (upper (upper-bound interval))
         )
         (round (* (/ (- upper lower) (+ upper lower)) 100))
    )
)

(define result (make-center-percent 3.5 10.0))

(check-equal? (center result) 3.5)

(check-equal? (percent result) 10.0)

(define result2 (make-center-percent 11.2 3.0))

(check-equal? (center result2) 11.2)

(check-equal? (percent result2) 3.0)