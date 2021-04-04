#lang sicp

(#%require rackunit)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))
)

(gcd 206 40)

; аппликативный порядок - 4 раза

; (gcd 206 40)

; (gcd 206 40)
; (if (= 40 0)
;     206
;     (gcd 40 (remainder 206 40))) => 1

; (gcd 40 6)
; (if (= 6 0)
;     40
;     (gcd 6 (remainder 40 6))) => 2


; (gcd 6 4)
; (if (= 4 0)
;     6
;     (gcd 4 (remainder 6 4))) => 3

; (gcd 4 2)
; (if (= 2 0)
;     4
;     (gcd 4 (remainder 4 2))) => 4

; итого 4 раза

; нормальный порядок - 18 раз