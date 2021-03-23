#lang sicp

(#%require rackunit)

; если шаг_изменения_приближения составляет менее 0.0001 от предыдущего приближения, пора перестать, слишком маленькие шаги, изменения
(define (good_enough? previous_guess current_guess)
    (< (abs (/ (- current_guess previous_guess)
            previous_guess)
        )
        0.001
    )
)

(define (average x y) 
    (/ (+ x y) 2)
)

(define (improve_guess current_guess target_value)
    (average current_guess (/ target_value current_guess)) ; среднее между числом-приближением и частным x/y
)

; Альтернативный подход к реализации good-enough? состоит в том, чтобы следить, как от одной итерации к другой изменяется guess, и остановиться, когда изменение оказывается небольшой долей значения приближения.

(define (sqrt_iter previous_guess guess_num target_num)
    (if (good_enough? previous_guess guess_num) ; если число-приближение изменилось на небольшую долю значения приближения, пора остановиться и
         guess_num ; возвращаем его (базовый случай)
         (sqrt_iter guess_num  ; <== текущее приближение становится прошлым
                    (improve_guess guess_num target_num) ; <== вычисляем новое приближение
                    target_num) ; иначе корректируем число-приближение и рекурсивно вызываем итерацию с новым числом
    )
)

(define (sqrt target)
    (sqrt_iter 0.0 1.0 target)
)

(sqrt 9)
(sqrt 25)
(sqrt 100)

(check-equal? (round (sqrt 9)) 3.0)
(check-equal? (round (sqrt 25)) 5.0)
(check-equal? (round (sqrt 100)) 10.0)