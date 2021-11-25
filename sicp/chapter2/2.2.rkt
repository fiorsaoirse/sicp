#lang sicp

(#%require rackunit)

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

(define (average x y)
    (/ (+ x y) 2.0)
)

; (define (midpoint-segment segment)
;     (let (
;             (middle-x-point (average (x-point (start-segment segment)) (x-point (end-segment segment))))
;             (middle-y-point (average (y-point (start-segment segment)) (y-point (end-segment segment))))
;          )
;          (make-point middle-x-point middle-y-point)
;     )
; )

(define (midpoint-segment segment)
    (let (
            (x1 (x-point (start-segment segment)))
            (x2 (x-point (end-segment segment)))
            (y1 (y-point (start-segment segment)))
            (y2 (y-point (end-segment segment)))
        )
        (make-point (average x1 x2) (average y1 y2))
    )
)

(define a-point (make-point -1 3))

(print-point a-point)

(check-equal? (x-point a-point) -1)
(check-equal? (y-point a-point) 3)

(define b-point (make-point 6 5))

(print-point b-point)

(check-equal? (x-point b-point) 6)
(check-equal? (y-point b-point) 5)

(define segment1 (make-segment a-point b-point))

(define middle-point (midpoint-segment segment1))

(print-point middle-point)

(check-equal? (x-point middle-point) 2.5)
(check-equal? (y-point middle-point) 4.0)
