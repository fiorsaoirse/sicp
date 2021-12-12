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

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (filter predicate sequance)
  (if (null? sequance)
      nil
      (let (
              (head (car sequance))
              (tail (cdr sequance))
           )
        (if (predicate head)
            (cons head (filter predicate tail))
            (filter predicate tail)
        )
      )
  )
)

(define (find-nums n s)
  (filter (lambda (sub-sequance)
            (let (
                  (sum (accumulate + 0 sub-sequance))
                 )                 
                 (= s sum)
            )
          )
    (flatmap 
        (lambda (i)
            (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                        (enumerate-interval 1 n)
                    )
                )
                (enumerate-interval 1 n)
            )
        )
        (enumerate-interval 1 n)
    )
  )
)

(check-equal? (find-nums 3 4) (list (list 1 1 2) (list 1 2 1) (list 2 1 1)))

(check-equal? (find-nums 5 3) (list (list 1 1 1)))