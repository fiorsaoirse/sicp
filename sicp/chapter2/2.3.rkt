#lang sicp

(#%require rackunit)

; utils

(define (square x) (* x x))

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
    (average current_guess (/ target_value current_guess))
)

(define (sqrt_iter previous_guess guess_num target_num)
    (if (good_enough? previous_guess guess_num)
         guess_num 
         (sqrt_iter guess_num  
                    (improve_guess guess_num target_num) 
                    target_num) 
    )
)

(define (sqrt target)
    (round (sqrt_iter 0.0 1.0 target))
)

; точки

(define (make-point x y)
    (cons x y)
)

(define (x-point point)
    (car point)
)

(define (y-point point)
    (cdr point)
)

(define (print-point point)
    (newline)
    (display "(")
    (display (x-point point))
    (display ", ")
    (display (y-point point))
    (display ")")
    (newline)
)

; отрезки

(define (make-segment x-point y-point)
    (cons x-point y-point)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (segment-length segment)
    (let (
            (x1 (x-point (start-segment segment)))
            (x2 (x-point (end-segment segment)))
            (y1 (y-point (start-segment segment)))
            (y2 (y-point (end-segment segment)))
        )
        (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))
    )
)

; интерфейсы,используют абстракцию "прямоугольник"

(define (rectangle-perimeter rectangle)
    (let (
            (width (rectangle-width rectangle))
            (height (rectangle-height rectangle))
         )

        (* (+ width height) 2)
    )
)

(define (rectangle-square rectangle)
    (let (
            (width (rectangle-width rectangle))
            (height (rectangle-height rectangle))
         )
        (* width height)
    )
)

; НИЖНИЙ слой абстракции, реализация прямоугольника

; вариант проще - прямоугольник через верхнюю левую точку, ширину и высоту

; (define (make-rectangle top-left-point width height)
;     (cons top-left-point (cons width height))
; )

; (define (rectangle-top-left-point rectangle)
;     (car rectangle)
; )

; (define (rectangle-width rectangle)
;     (car (cdr rectangle))
; )

; (define (rectangle-height rectangle)
;     (cdr (cdr rectangle))
; )

; (define rect (make-rectangle (make-point 1 2) 10 20))

; (check-equal? (rectangle-perimeter rect) 60)
; (check-equal? (rectangle-square rect) 200)

; вариант сложнее - представление через верхнюю левую точку и нижнюю правую точку

(define (make-rectangle top-left-point bottom-right-point)
    (cons top-left-point bottom-right-point)
)

(define (rectangle-top-left-point rectangle)
    (car rectangle)
)

(define (rectangle-bottom-right-point rectangle)
    (cdr rectangle)
)

(define (rectangle-top-right-point rectangle)
    (let (
            (A (rectangle-top-left-point rectangle))
            (B (rectangle-bottom-right-point rectangle))
         )

        (make-point (x-point B) (y-point A))
    )
)

(define (rectangle-width rectangle)
    (let (
            (A (rectangle-top-left-point rectangle))
            (B (rectangle-top-right-point rectangle))
         )

         (segment-length (make-segment A B))
    )
)

(define (rectangle-height rectangle)
    (let (
            (A (rectangle-top-right-point  rectangle))
            (B (rectangle-bottom-right-point rectangle))
         )

         (segment-length (make-segment A B))
    )
)

(define rect (make-rectangle (make-point 1 22) (make-point 11 2)))

(check-equal? (rectangle-perimeter rect) 60.0)
(check-equal? (rectangle-square rect) 200.0)