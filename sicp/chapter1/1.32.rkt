#lang sicp

(#%require rackunit)

; аккумулирование значений в интервале от a до b

; рекурсивный вариант

(define (accumulate-rec combiner null-value a b term next)
    (if (> a b)
        null-value
        (combiner (term a)
            (accumulate-rec combiner null-value (next a) b term next)
        )
    )
)

; итеративный вариант

(define (accumulate-iter combiner null-value a b term next)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))
        )
    )

    (iter a null-value)
)

(define (sum x y) (+ x y))

(define (product x y) (* x y))

(define (inc x) (+ x 1))

(check-equal? (accumulate-rec sum 0 10 20 identity inc) 165)
(check-equal? (accumulate-rec sum 0 2 5 identity inc) 14)
(check-equal? (accumulate-rec product 1 2 5 identity inc) 120)
(check-equal? (accumulate-rec product 1 3 6 identity inc) 360)

(check-equal? (accumulate-iter sum 0 10 20 identity inc) 165)
(check-equal? (accumulate-iter sum 0 2 5 identity inc) 14)
(check-equal? (accumulate-iter product 1 2 5 identity inc) 120)
(check-equal? (accumulate-iter product 1 3 6 identity inc) 360)