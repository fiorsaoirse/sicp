#lang sicp

(#%require rackunit)

; ---- bottom level of the abstraction 
; constructor
(define (make-vect x-point y-point)
    (cons x-point y-point)
)

; selector for x-point
(define (xcor-vect vector)
    (car vector)
)

;selector for y-point
(define (ycor-vect vector)
    (cdr vector)
)

; ---- upper level of the abstractions
; ---- it uses constructors and selectors instead of pairs

(define (add-vect vector1 vector2)
    (let (
            (x-point (+ (xcor-vect vector1) (xcor-vect vector2)))
            (y-point (+ (ycor-vect vector1) (ycor-vect vector2)))
         )
         (make-vect x-point y-point)
    )
)

(define (sub-vect vector1 vector2)
    (let (
            (x-point (- (xcor-vect vector1) (xcor-vect vector2)))
            (y-point (- (ycor-vect vector1) (ycor-vect vector2)))
         )
         (make-vect x-point y-point)
    )
)

(define (scale-vect scale vector)
    (let (
            (x-point (* scale (xcor-vect vector)))
            (y-point (* scale (ycor-vect vector)))
         )
         (make-vect x-point y-point)
    )
)

(check-equal? (xcor-vect (make-vect 1 3)) 1)
(check-equal? (ycor-vect (make-vect 1 3)) 3)

(check-equal? (add-vect (make-vect 1 5) (make-vect 2 8)) (make-vect 3 13))
(check-equal? (sub-vect (make-vect 10 21) (make-vect 9 12)) (make-vect 1 9))
(check-equal? (scale-vect 3 (make-vect 3 6)) (make-vect 9 18))