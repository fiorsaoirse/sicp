#lang sicp

(#%require rackunit)

(define x (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define y (list 1 (list 4 (list 9 16) 25) (list 36 49)))

(define nil '())

(define (square x) (* x x))

(define (square-tree tree)
    (cond ((null? tree) nil)
          ((pair? tree) (cons (square-tree (car tree))
                              (square-tree (cdr tree))
                        )
          )
          (else (square tree))
    )
)

(check-equal? (square-tree x) y)

(define (square-tree-map tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree-map sub-tree)
              (square sub-tree)
          )
        )
        tree
  )
)

(check-equal? (square-tree-map x) y)
