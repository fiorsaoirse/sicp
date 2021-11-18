#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (compose f g)
    (lambda (x)
        (f (g x))
    )
)

(define (repeated f count)
    (lambda (x)
        (define (inner n)
            (if (= n 1)
                f
                (compose f (inner (dec n)))
            )
        )

        ; (inner count) в результате получает лямбду вида f(f(f(... f(x) .... )))
        ((inner count) x)
    )
)

(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated square 5) 2) 4294967296)
(check-equal? ((repeated inc 3) 5) 8)