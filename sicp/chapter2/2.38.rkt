#lang sicp

(#%require rackunit)

(define nil '())

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

(check-equal? (fold-right / 1 (list 1 2 3)) 3/2)

(check-equal? (fold-left / 1 (list 1 2 3)) 1/6)

(check-equal? (fold-right list nil (list 1 2 3)) (list 1 (list 2 (list 3 nil))))

(check-equal? (fold-left list nil (list 1 2 3)) (list (list (list nil 1) 2) 3))