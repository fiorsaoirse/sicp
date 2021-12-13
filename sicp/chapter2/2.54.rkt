#lang sicp

(#%require rackunit)

(define (equal? list1 list2) 
    (if (and (null? list1) (null? list2))
        #t    
        (let (
                (car1 (car list1))
                (car2 (car list2))
             )
                     
             (if (not (eq? car1 car2))
                 #f
                 (equal? (cdr list1) (cdr list2))
             )
        )
    )
)

(define (equal-x? a b)
    (or 
        (and (null? a) (null? b))
        (and (number? a) (number? b) (= a b))
        (and (symbol? a) (symbol? b) (eq? a b))
        (and 
            (pair? a)
            (pair? b)
            (and (equal-x? (car a) (car b))
                 (equal-x? (cdr a) (cdr b))
            )             
        )
    )
)

(define x (list 1 's 3 '()))
(define y (list 1 '(ad (das) d1) '3 '()))

(check-equal? (equal? '(this is a list) '(this is a list)) #t)
(check-equal? (equal? '(this is a list) '(this (is a) list)) #f)

(check-equal? (equal-x? x x) #t)
(check-equal? (equal-x? x y) #f)