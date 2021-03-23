#lang sicp

(#%require rackunit)

(define (plus a b)
    (if (= a 0)
        b
        (inc (plus (dec a) b))
    )
)

(define (plus2 a b)
    (if (= a 0)
        b
        (plus2 (dec a) (inc b))
    )
)

; Вместо "+" будем использовать название процедуры plus и plus2 - для понимания
; Вычислить (plus 4 5)

; (plus 4 5)

; (if (= 4 0)
;     5
;     (inc ( plus (dec 4) 5) )
; )

; => вычисление (inc (plus (dec 4) 5)) => (inc (plus 3 5))

; (if (= 3 0)
;     5
;     (inc ( plus (dec 3) 5) )
; )

; => вычисление (inc (plus (dec 3) 5)) => (inc (plus 2 5))

; (if (= 2 0)
;     5
;     (inc ( plus (dec 2) 5) )
; )

; => вычисление (inc (plus (dec 2) 5)) => (inc (plus 1 5))

; (if (= 1 0)
;     5
;     (inc ( plus (dec 1) 5) )
; )

; => вычисление (inc (plus (dec 1) 5)) => (inc (plus 0 5))

; (if (= 0 0)
;     5
;     (inc ( plus (dec 1) 5) )
; )

; => итоговое вычисление (inc (plus 4 (inc (plus 3 (inc (plus 2 (inc (plus 1 (inc (plus 0 5))))))))))
; получилась линейно-рекурсивная процедура

; Вычислить (plus2 4 5)

; (if (= 4 0)
;     5
;     (plus2 (dec 4) (inc 5))
; )

; => вычисление (plus2 (dec 4) (inc 5)) => (plus2 3 6)
; => вычисление (plus2 (dec 3) (inc 6)) => (plus2 2 7)
; => вычисление (plus2 (dec 2) (inc 7)) => (plus2 1 8)
; => вычисление (plus2 (dec 1) (inc 8)) => (plus2 0 9)

; получилась линейно-итеративная процедура