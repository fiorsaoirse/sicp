#lang sicp

(#%require rackunit)

(define (for-each term items)
    (cond ((not (null? items))
                (term (car items))
                (for-each term (cdr items))
          )
    )
)

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))