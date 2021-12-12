#lang sicp

(#%require rackunit)

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2)
)

(define (origin-frame frame)
    (car frame)
)

(define (edge1-frame frame)
    (car (cdr frame))
)

(define (edge2-frame frame)
    (car (cdr (cdr frame)))
)

(check-equal? (origin-frame (make-frame 1 2 3)) 1)
(check-equal? (edge1-frame (make-frame 1 2 3)) 2)
(check-equal? (edge2-frame (make-frame 1 2 3)) 3)

(define (make-frame-2 origin edge1 edge2)
    (cons origin (cons edge1 edge2))
)

(define (origin-frame-2 frame)
    (car frame)
)

(define (edge1-frame-2 frame)
    (car (cdr frame))
)

(define (edge2-frame-2 frame)
    (cdr (cdr frame))
)

(check-equal? (origin-frame-2 (make-frame-2 1 2 3)) 1)
(check-equal? (edge1-frame-2 (make-frame-2 1 2 3)) 2)
(check-equal? (edge2-frame-2 (make-frame-2 1 2 3)) 3)