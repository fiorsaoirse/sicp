#lang sicp

(#%require rackunit)

(define (cont-frac n d k) 
    (define (iter i) 
      (/ (n i) (+ (d i) 
                  (if (< i k) 
                      (iter (+ i 1)) 
                      0)))) 
    (iter 1)
)

(define (tan-cf x k)
    (cont-frac 
        (lambda (i) 
            (if (= i 1)
                i
                (* i i)
            )
        )
        (lambda (i) (- (* 2 i) 1)
        k
    )
)