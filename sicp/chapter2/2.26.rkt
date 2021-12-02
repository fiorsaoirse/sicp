#lang sicp

(#%require rackunit)

(define x (list 1 2 3))
(define y (list 4 5 6))

; (append x y)
; мерж двух списков в один
; (1 2 3 4 5 6)

(display (cons x y))
; новый список, элементом которого является первый список
; ((1 2 3) 4 5 6)

(newline)

(display (list x y))
; список из 2 элементов, где первый элемент - первый список, второй элемент - второй список
; (cons ( (1 2 3) (4 5 6) )

(newline)