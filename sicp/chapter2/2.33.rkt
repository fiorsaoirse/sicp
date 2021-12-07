#lang sicp

(#%require rackunit)

(define nil '())

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq))
      )
  )
)

(define (map proc sequence)
    (accumulate (lambda (x y) (cons (proc x) y))
                nil
                sequence)
)

(define (append seq1 seq2)
    (accumulate cons seq2 seq1)
)

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence)
)

(check-equal? (map (lambda (x) (* 2 x)) (list 1 2 3 4 5))
              (list 2 4 6 8 10)
)

(check-equal? (append (list 1 2 3 4 5) (list 6 7 8 9 10))
              (list 1 2 3 4 5 6 7 8 9 10)
)

(check-equal? (length (list 1 2 3)) 3)