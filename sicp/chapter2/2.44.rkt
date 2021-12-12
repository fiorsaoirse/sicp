#lang sicp

(#%require rackunit)

; в данной задаче реализация below и beside не требуются
; здесь определения нужны для того, чтобы не было ошибки unbound indentifier
(define (below x y) '())

(define (beside x y) '())

(define (up-split painter n)
    (if (= 0 n)
        painter
        (let (
                (upper-painter (up-split (- n 1)))
             )             
            (below painter (beside upper-painter upper-painter))
        )
    )
)