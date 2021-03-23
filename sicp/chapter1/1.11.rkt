#lang sicp

(#%require rackunit)

; Функция f определяется правилом f(n) = n, если n < 3 и f(n) = f(n - 1) + f(n - 2) + f(n - 3)

; Линейно-рекурсивный процесс

(define (lin-rec n)
    (if (< n 3)
        n
        (+ (lin-rec (- n 1))
           (lin-rec (- n 2))
           (lin-rec (- n 3))
        )
    )
)

; Линейно-итеративный процесс

(define (lin-iter n)
    (define (lin-inner a b c counter)
        (if (= counter 0)
            c
            (lin-inner (+ a b c) a b (- counter 1))
        )
    )
    
    (lin-inner 2 1 0 n)
)

(check-equal? (lin-rec 5) 11)
(check-equal? (lin-iter 5) 11)