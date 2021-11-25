#lang sicp

(#%require rackunit)

(define zero
    (lambda (f)
        (lambda (x) x)
    )
)

(define (add-1 num)
    (lambda (f)
        (lambda (x) (f ((num f) x)))
    )
)

(define one
    (lambda (f)
        (lambda (x) (f x))
    )
)


(define two
    (lambda (f)
        (lambda (x) (f (f x))
    )
)