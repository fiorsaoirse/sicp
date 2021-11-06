#lang sicp

(#%require rackunit)

; вычисление произведения целых чисел в диапазоне от a до b

; рекурсивный вариант

(define (product-rec a b term next)
    (if (> a b)
        1
        (* (term a)
            (product-rec (next a) b term next)
        )
    )
)

; итеративный вариант

(define (product-iter a b term next)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))
        )
    )

    (iter a 1)
)

(define (identity x) x)

(define (inc x) (+ x 1))

(check-equal? (product-rec 10 15 identity inc) 3603600)
(check-equal? (product-rec 2 8 identity inc) 40320)

(check-equal? (product-iter 10 15 identity inc) 3603600)
(check-equal? (product-iter 2 8 identity inc) 40320)

(define (factorial x)
    (product-iter 1 x identity inc)
)

(check-equal? (factorial 3) 6)
(check-equal? (factorial 5) 120)
(check-equal? (factorial 7) 5040)

(define (pi n) 
    (define (square x) (* x x))

    (define (pi-term k) 
        (/ (* (- k 1) (+ k 1)) (square k))
    ) 

    (define (pi-next k) 
        (+ k 2)
    ) 

    (* 4 (product-rec pi-term 3 pi-next n))
)