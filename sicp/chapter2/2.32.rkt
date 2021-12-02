#lang sicp

(#%require rackunit)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) )
  )
)

(define (subsets s)
    (if (null? s)
        (list s)
        (let (
                (rest (subsets (cdr s)))
                (value (car s))
             )
             (append rest (map 
                           (lambda (inner-set)
                                (cons value inner-set)
                           )
                           rest
                           )
             )
        )
    )
)

(check-equal? (subsets (list 1 2 3)) (list (list) (list 3) (list 2) (list 2 3) (list 1) (list 1 3) (list 1 2) (list 1 2 3)) )