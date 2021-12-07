#lang sicp

(#%require rackunit)

(define nil '())

(define x
    (list (list 1 2 3)
          (list 4 5 6)
          (list 7 8 9)
          (list 10 11 12)
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

(check-equal? (accumulate-n + 0 x) (list 22 26 30))
