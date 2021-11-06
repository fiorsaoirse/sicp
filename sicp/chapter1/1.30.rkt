#lang sicp

(#%require rackunit)

; вычисление суммы целых чисел в диапазоне от a до b

(define (sum a b term next)
    (define (iter curr result)
        (if (> curr b)
            result
            (iter (next curr) (+ (term curr) result))
        )
    )
    
    (iter a 0)
)

(define (identity x) x)

(define (inc x) (+ x 1))

(check-equal? (sum 13 19 identity inc) 112)
(check-equal? (sum 10 13 identity inc) 46)
(check-equal? (sum 2 15 identity inc) 119)