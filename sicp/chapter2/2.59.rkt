#lang sicp

(#%require rackunit)

(define (element-of-set? element set)
    (cond
        ((null? set) #f)
        ((equal? element (car set)) #t)
        (else (element-of-set? element (cdr set)))
    )
)

(define (adjoin-set element set)
    (if (element-of-set? element set)
        set
        (cons element set)
    )
)

(define (intersection-set set1 set2)
    (cond 
         ((or (null? set1) (null? set2)) '())
         ((element-of-set? (car set1) set2)
            (cons (car set1)
                  (intersection-set (cdr set1) set2)
            )
         )
         (else (intersection-set (cdr set1) set2))
    )
)

(define (union-set set1 set2)
    (cond
            ((null? set1) set2)
            ((null? set2) set1)
            (else (union-set (cdr set1) (adjoin-set (car set1) set2)))
    )
)

(define set1 '(1 a 2 b 3 c))
(define set2 '(1 q 5 b 3 m))
(define set3 '())

(check-equal? (adjoin-set 'foo set1) '(foo 1 a 2 b 3 c) )

(check-equal? (union-set set1 set2) '(c 2 a 1 q 5 b 3 m))

(check-equal? (union-set set2 set3) '(1 q 5 b 3 m))

(check-equal? (union-set set1 set3) '(1 a 2 b 3 c))

(check-equal? (union-set set3 set3) '())