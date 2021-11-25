#lang sicp

(#%require rackunit)

(define (dec x) (- x 1))

(define (power base exponent)
    (define (power-iter counter result)
        (if (= 1 counter)
            result
            (power-iter (dec counter) (* result base))
        )
    )

    (power-iter exponent base)
)

(define (factor value base) 
  (define (factor-iter value counter) 
    (if (= (remainder value base) 0) 
        (factor-iter (/ value base) (+ counter 1)) 
        counter)) 
  (factor-iter value 0)
)

(define (cons x y)
    (* (power 2 x) (power 3 y))
)

(define (car pair)
    (factor pair 2)
)

(define (cdr pair)
    (factor pair 3)
)

(define pair (cons 11 22))

(check-equal? (car pair) 11)
(check-equal? (cdr pair) 22)