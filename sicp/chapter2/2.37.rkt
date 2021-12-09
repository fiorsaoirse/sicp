#lang sicp

(#%require rackunit)

(define nil '())

(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq))
      )
  )
)

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      nil
      (let (
            (cars (map (lambda (x) (car x)) seqs))
            (cdrs (map (lambda (x) (cdr x)) seqs))
           )
          
          (cons 
            (accumulate op initial cars)
            (accumulate-n op initial cdrs)
          )
      )
  )
)

(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m)
)

(define (transpose mtx)
    (accumulate-n cons nil mtx)
)

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (row)
                (matrix-*-vector cols row)
             )
             m
        )
    )
)

(check-equal? (transpose matrix) (list (list 1 4 6) (list 2 5 7) (list 3 6 8) (list 4 6 9)))