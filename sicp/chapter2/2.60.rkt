#lang sicp

(#%require rackunit)

(define (element-of-set? element set)
    (cond
        ((null? set) #f)
        ((equal? element (car set)) #t)
        (else (element-of-set? element (cdr set)))
    )
)

; по сути - единственное измнение, т.к. нас больше не заботит проверка
; на существование элемента во множесте
(define (adjoin-set element set)
    (cons element set)
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

(define set1 '(1 a 1 2 b 3 a 1 a c))
(define set2 '(1 q 5 b 3 m))
(define set3 '())

(check-equal? (element-of-set? 'foo set1) #f)
(check-equal? (element-of-set? 'a set1) #t)

(check-equal? (adjoin-set 'foo set1) '(foo 1 a 1 2 b 3 a 1 a c))

(check-equal? (intersection-set set1 set2) '(1 1 b 3 1))
(check-equal? (intersection-set set2 set3) '())
(check-equal? (intersection-set set1 set3) '())
(check-equal? (intersection-set set3 set3) '())

(check-equal? (union-set set1 set2) '(c a 1 a 3 b 2 1 a 1 1 q 5 b 3 m))
(check-equal? (union-set set2 set3) '(1 q 5 b 3 m))
(check-equal? (union-set set1 set3) '(1 a 1 2 b 3 a 1 a c))
(check-equal? (union-set set3 set3) '())