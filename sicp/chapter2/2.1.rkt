#lang sicp

(#%require rackunit)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))
)

(define (abs x)
        (if (>= x 0)
            x
            (* -1 x)
        )
)

(define (sign x)
        (cond ((> x 0) 1)
              ((< x 0) -1)
              (else 0)
        )
)

(define (make-rat num den)
    (let ((g (* (gcd (abs num) (abs den)) (sign den))))
        (cons (/ num g) (/ den g))
    )
)

(define (numer rational)
    (car rational)
)

(define (denom rational)
    (cdr rational)
)

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline)
)

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))
    )
)

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))
    )
)

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))
    )
)

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))
    )
)

(define (equal-rat? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x)))
)

(define one-half (make-rat 1 2))

(check-equal? (numer one-half) 1)
(check-equal? (denom one-half) 2)

(define one-third (make-rat 1 3))

(check-equal? (numer one-third) 1)
(check-equal? (denom one-third) 3)

(define two-third (add-rat one-third one-third))

(check-equal? (numer two-third) 2)
(check-equal? (denom two-third) 3)

(define negative-six-eleven (make-rat -6 11))

(check-equal? (numer negative-six-eleven) -6)
(check-equal? (denom negative-six-eleven) 11)
