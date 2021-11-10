#lang sicp

(#%require rackunit)

; линейно-рекурсивный порядок вычисления
(define (cont-frac n d k) 
    (define (iter i) 
      (/ (n i) (+ (d i) 
                  (if (< i k) 
                      (iter (+ i 1)) 
                      0)))) 
    (iter 1)
)

; линейно-итеративный порядоко вычисления
(define (cont-frac-iter n d k)
    (define (iter i result)
        ; считаем новое значение
        (let (
                (newResult (/ (n i) (+ (d i) result))) 
                (newI (- i 1))
             )

            ; тело
            (if (<= newI 0)
                newResult
                (iter (dec i) newResult)
            )
        )
    )

    (iter k 0)
)

(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)) 

(/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100)) 