#lang sicp

(#%require rackunit)

(define (square x) (* x x))

; если модуль разницы квадрата числа-приближения и числа, из которого ищем квадратный корень меньше установленной погрешности, тогда число-приближение подходит, иначе нет
(define (good_enough? guess_num target_num)
    (< (abs 
        (- (square guess_num) target_num)) 
        0.0001
    )
)

(define (average x y) 
    (/ (+ x y) 2)
)

(define (improve_guess prevoius_value target_value)
    (average prevoius_value (/ target_value prevoius_value)) ; среднее между числом-приближением и частным x/y
)

(define (sqrt_iter guess_num target_num)
    (if (good_enough? guess_num target_num) ; если число-приближение подходит в рамках погрешности
         guess_num ; возвращаем его (базовый случай)
         (sqrt_iter (improve_guess guess_num target_num) target_num) ; иначе корректируем число-приближение и рекурсивно вызываем итерацию с новым числом
    )
)

(define (sqrt target)
    (sqrt_iter 1.0 target)
)

(sqrt 9)

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)
    )
)

(check-equal? (new-if (= 2 3) 1 0) 0)

(check-equal? (new-if (= 2 2) 1 0) 1)

(define (new-sqrt-iter guess_num target_num)
    (
        new-if (good_enough? guess_num target_num)
                guess_num
                (new-sqrt-iter (improve_guess guess_num target_num) target_num)
    )
)

(define (new-sqrt x)
    (
        new-sqrt-iter 1.0 x
    )
)

; (new-sqrt 9) 
; new-if это функция. т.к. вычисление идет аппликативно, дожны быть вычислены ВСЕ аргументы, в т.ч. else-clause - а вот уже в нем идет рекурсивный вызов 
; new-if, второй операнд не может быть вычислен. поэтому оператор new-if никогда не будет вызван (соответственно, никогда не будет проверен cond)

;(define (new_sqrt_iter guess x) 
;  (new-if (good-enough? guess x)   
;          guess 
;          (new_sqrt_iter (improve guess x) 
;                     x)))   <=== (new_sqrt_iter 1.0 4) можно развернуть до (new-if false 1.0 рекурсивный-вызов)
