#lang sicp

(#%require rackunit)

(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) )
  )
)

(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (fold-right op initial (cdr seq))
      )
  )
)

; по сути - привычный reduce, свертка с левого элемента вправо (последний элемент)
; последовательности с результатом всех обработок предыдущих элементов
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter 
                (op result (car rest))
                (cdr rest)
            )
        )
    )

    (iter initial sequence)
)

(define (reverse-right sequence)
    (fold-right (lambda (first rest) (append rest (list first))) nil sequence)
)

(define (reverse-left sequence)
    (fold-left (lambda (first rest) (cons rest first)) nil sequence)
)

(check-equal? (reverse-right (list 1 4 9 16 25)) (list 25 16 9 4 1))

(check-equal? (reverse-left (list 1 4 9 16 25)) (list 25 16 9 4 1))