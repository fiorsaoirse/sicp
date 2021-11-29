#lang sicp

(#%require rackunit)

; линейно-рекурсивный процесс
; ПРИМЕЧАНИЕ - не совсем правильно из-за структуры списка, в итоге получится пара, а не список
;;; (define (reverse items)
;;;     (if (null? items)
;;;         (list)
;;;         (cons (reverse (cdr items)) (car items))
;;;     )
;;; )

; линейно-итеративный процесс
(define (reverse-iter items)
    (define (iter source result)
        (if (null? source)
            result
            (iter (cdr source) (cons (car source) result) )
        )
    )

    (iter items (list))
)

; (check-equal? (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))

(check-equal? (reverse-iter (list 1 4 9 16 25)) (list 25 16 9 4 1))