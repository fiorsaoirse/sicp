#lang sicp

(#%require rackunit)

; const cons = (x, y) => func => func(x, y)
; в пару возвращает процедуру, которая получает другую процедуру и передает в нее
; параметры (x и y)

(define (cons x y)
    (lambda (fn) (fn x y))
)

; const car = pair => pair((x, y) => x)

(define (car pair)
    (pair (lambda (x y) x))
)

; const cdr = pair => pair((x, y) => y)

(define (cdr pair)
    (pair (lambda (x y) y))
)

(define pair (cons 11 22))

(check-equal? (car pair) 11)
(check-equal? (cdr pair) 22)