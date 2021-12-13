#lang sicp

(#%require rackunit)

(define nil '())

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

; ---- upper level of abstraction

(define (make-segment vector1 vector2)
    (cons vector1 vector2)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

; в данной задаче реализация этого метода не требуется,
; учитываем только то, что он рисует на экране линию (отрезок) между 2 указанными точками
(define (draw-line start-point end-point)
    nil
)

(define (frame-coord-map frame)
    nil
)

(define (for-each term items)
    (cond ((not (null? items))
                (term (car items))
                (for-each term (cdr items))
          )
    )
)

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each 
        (lambda (segment)
            (draw-line 
                ((frame-coord-map frame) (start-segment segment))
                ((frame-coord-map frame) (end-segment segment))
            )
        )
        segment-list)
    )
)

(define outline-painter 
  (segments->painter (list (make-segment (make-vect 0 0) 
                                         (make-vect 1 0)) 
                           (make-segment (make-vect 1 0) 
                                         (make-vect 1 1)) 
                           (make-segment (make-vect 1 1) 
                                         (make-vect 0 1)) 
                           (make-segment (make-vect 0 1) 
                                         (make-vect 0 0))
                        )
  )
)

(define x-painter 
  (segments->painter (list (make-segment (make-vect 0 0) 
                                         (make-vect 1 1)) 
                           (make-segment (make-vect 1 0) 
                                         (make-vect 0 1))
                     )
  )
)