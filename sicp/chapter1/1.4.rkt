#lang sicp

(#%require rackunit)

(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b)
)


; в качестве возврата из блока if вернется оператор, который будет применяться к параметрам a и b
; в случае, если b больше 0, результатом будет сложение a и b, в случае, если меньше нуля, будет разность

(check-equal? (a-plus-abs-b 1 2) 3)
(check-equal? (a-plus-abs-b 1 -2) 3)