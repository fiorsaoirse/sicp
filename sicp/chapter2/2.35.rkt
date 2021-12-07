#lang sicp

(#%require rackunit)

(define x (list 1 (list 3 (list 1 4) 5) (list 6 7)))
(define y (list 1 (list 9 (list 1 16) 25) (list 36 49 11)))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq))
      )
  )
)

(define (count-leaves tree)
    (accumulate +
                0
                (map (lambda (sub-tree)
                    (if (pair? sub-tree)
                        (count-leaves sub-tree)
                        1
                    )
                ) tree)
    )
)

(check-equal? (count-leaves x) 7)
(check-equal? (count-leaves y) 8)