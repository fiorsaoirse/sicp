#lang sicp

(#%require rackunit)

(define tolerance 0.00001)

(define (fixed-point transform-function first-guess)
    ; предикат, способный вычислить базовый случай
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance)
    )

    ; внутренняя рекурсивная функция
    (define (try current-guess)
        ; пусть next равен f(x), где f - трансформирующая функция, x - текущее значение guess
        (let ((next (transform-function current-guess)))
            ; сравнивается текущая точка и следующая точка, если расстояние между ними меньше
            ; приближения, считаем, что ответ найден, иначе - идем глубже
            (if (close-enough? next current-guess)
                next
                (try next)
            )
        )
    )

    (try first-guess)
)


(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
