#lang sicp

(#%require rackunit)

(define result 
    (list 1 (list 2 (list 3 4)))
)

; (1 (2 (3 4)))
(display result) 
(newline)