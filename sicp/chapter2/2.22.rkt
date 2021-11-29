#lang sicp

(#%require rackunit)

(define nil '())

(define (square x) (* x x))

(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                   (cons (square (car things)) answer))
        )
    )

    (iter items nil)
)

; В данном случае первый элемент в списке-источнике становится последним элементом
; в списке-результате, а список-результат идет в качестве cdr пары 
; (cons (square (car things)) answer))

; (1 2 3 4) -> ()
; (2 3 4) -> (1)
; (3 4) -> (4 1)
; (4) -> (9 4 1)
; () -> (16 9 4 1)

(define (square-list2 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                   (cons answer (square (car things))))
        )
    )

    (iter items nil)
)

; В данном случае порядок будет корректный, сначала идет список-результат, затем 
; результат преобразования над головой списка, однако,
; именно списком типа list можно назвать такой список, хвост последнего элемента в котором
; будет являться NULL, т.е. формата (1 . (4 . (9 . (16 . NULL))))
; Однако, в данной процедуре посчитанное значение становится хвостом пары, а в качестве головного узла 
; всего "списка" будет узел (NULL . 1) и общий список будет иметь формат
; ((((NULL . 1) . 4) . 9) . 16), что не является
; списком типа list