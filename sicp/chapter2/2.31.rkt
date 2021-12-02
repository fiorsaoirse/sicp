#lang sicp

(#%require rackunit)

(define x (list 1 (list 3 (list 1 4) 5) (list 6 7)))
(define y (list 1 (list 9 (list 1 16) 25) (list 36 49)))

(define nil '())

(define (square x) (* x x))

(define (tree-map fn tree)
    (cond ((null? tree) nil)
          ((pair? tree) (cons (tree-map fn (car tree))
                              (tree-map fn (cdr tree))
                        )
          )
          (else (fn tree))
    )
)

(define (square-tree tree)
  (tree-map square tree)
)

(check-equal? (square-tree x) y)
