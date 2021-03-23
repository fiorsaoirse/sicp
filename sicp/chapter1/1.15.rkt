#lang sicp

(#%require rackunit)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0)))
    )
)

; 1.  (p(sine 4.05))
;     (p(sine 1.35))
;     (p(sine 0.45))
;     (p(sine 0.15))
;     (p(sine 0.05)) => развертка
; Всего вызов пройдет 5 раз

; 2. O(log n)