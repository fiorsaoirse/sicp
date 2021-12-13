#lang sicp

(#%require rackunit)

(define (memq item sequance)
    (cond ((null? sequance) #f)
          ((eq? (car sequance) item) sequance)
          (else (memq item (cdr sequance)))    
    )
)

(list 'a 'b 'c)
; вывод (a b c)

(list (list 'george))
; вывод ((george))

(cdr '((x1 x2) (y1 y2)))
; перевод в список, элементами которого являются "(x1 x2)" и "(y1 y2)"
; cdr вернет фактически пару (cons "(y1 y2)" null) => которая является списком ("(y1 y2)")

(cadr '((x1 x2) (y1 y2)))
; перевод в список, элементами которого являются "(x1 x2)" и "(y1 y2)"
; cadr возвращает голову хвоста => т.е. надо взять голову от предыдущей операции
; (car (cdr '((x1 x2) (y1 y2))))) => "(y1 y2)"

(pair? (car '(a short list)))
; '(a short list) => (list 'a' 'short' 'list')
; (car (list 'a' 'short' 'list')) => 'a'
; вернет #f

(memq 'red '((red shoes) (blue socks)))
; '((red shoes) (blue socks)) => (list '(red shoes)' '(blue socks)' )
; подстроки 'red' в списке '(red shoes)', '(blue socks)' нет
; вернет #f

(memq 'red '(red shoes blue socks))
; '(red shoes blue socks) => (list 'red' 'shoes' 'blue' 'socks')
; подстрока 'red' есть в списке
; вернет список (list 'red' 'shoes' 'blue' 'socks')
