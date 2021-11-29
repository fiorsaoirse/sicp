#lang sicp

(#%require rackunit)

(define (last-pair list)
    (let ((tail (cdr list)))
        (if (null? tail)
            (car list)
            (last-pair tail)
        )
    )
)

(check-equal? (last-pair (list 23 72 149 34)) 34)

(check-equal? (last-pair (list 11)) 11)