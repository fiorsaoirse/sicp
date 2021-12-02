#lang sicp

(#%require rackunit)

(define first (list 1 3 (list 5 7) 9))

; комбинация - (cons 1 (cons 3 (cons (list 5 7) (cons 9 null))))
; чтобы вытащить 7:
; (cdr first) => one-inner (cons 3 (cons (list 5 7) (cons 9 null)))
; (cdr one-inner) => two-inner (cons (list 5 7) (cons 9 null))
; (car two-inner) => three-inner (list 5 7)
; (cdr three-inner) => four-inner (cons 7 null)
; (car four-inner) => 7


(check-equal? (car (cdr (car (cdr (cdr first))))) 7)

(define second (list (list 7)))
; комбинация (cons (cons (7 null)) null)
; (car second) => one-inner (cons (7 null))
; (car one-inner) => 7

(check-equal? (car (car second)) 7)

(define third (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; комбинация (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 null)))))))
; (cdr third) => one-inner ((cons 2 (cons 3 (cons 4 (cons 5 (cons 6 7))))))
; (car one-inner) => two-inner (cons 3 (cons 4 (cons 5 (cons 6 7))))
; (cdr two-inner) => three-inner (cons 4 (cons 5 (cons 6 7)))
; (car three-inner) => four-inner ((cons 5 (cons 6 (cons 7 null))))
; и т.д.

(check-equal? (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third)))))))))))) 7)