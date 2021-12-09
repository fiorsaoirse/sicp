#lang sicp

(#%require rackunit)

(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) )
  )
)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq))
      )
  )
)

(define (enumerate-interval from to)
    (if (= 0 to)
        nil
        (cons from (enumerate-interval (+ from 1) (- to 1)))
    )
)

(check-equal? (enumerate-interval 1 6) (list 1 2 3 4 5 6))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (unique-pairs n)
    (flatmap 
        (lambda (i)
            (map 
              (lambda (j) (list i j))
              (enumerate-interval 1 (- i 1))
            )
            
        )
        (enumerate-interval 1 n)
    )
)

(check-equal? (unique-pairs 6)
    (list 
        (list 2 1)
        (list 3 1)
        (list 3 2)
        (list 4 1)
        (list 4 2)
        (list 4 3)
        (list 5 1)
        (list 5 2)
        (list 5 3)
        (list 5 4)
        (list 6 1)
        (list 6 2)
        (list 6 3)
        (list 6 4)
        (list 6 5)
    )
)