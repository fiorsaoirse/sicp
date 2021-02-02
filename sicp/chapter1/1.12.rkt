#lang sicp

(#%require rackunit)

(define (pascal a b)
    (cond ((or (= a b) (= b 0)) 1)
          (else (+ (pascal (- a 1) (- b 1))
                   (pascal (- a 1) b)
          ))
    )
)

(check-equal? (pascal 4 2) 6)
(check-equal? (pascal 5 3) 10)
(check-equal? (pascal 4 3) 4)
