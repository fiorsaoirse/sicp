#lang sicp

(#%require rackunit)

(define (element-of-set? element set)
    (cond
        ((null? set) #f)
        ((< element (car set)) #f)
        ((= element (car set)) #t)        
        (else (element-of-set? element (cdr set)))
    )
)

(define (adjoin-set element set)
    (if (element-of-set? element set)
        set
        (let (
                (head (car set))
             )
             
             (cond 
                    ((> element head) (cons head (adjoin-set element (cdr set))))
                    ((< element head) (cons element set))
                    (else (error "Ooops"))
             )
        )
    )
)

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let (
                (x1 (car set1))
                (x2 (car set2))
             )

             (cond 
                ((= x1 x2)
                    (cons x1 (intersection-set (cdr set1) (cdr set2)))
                )
                ((< x1 x2)
                    (intersection-set (cdr set1) set2)
                )
                ((> x1 x2)
                    (intersection-set set1 (cdr set2))
                )
             )
        )
    )

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
            (else 
                 (let (
                         (x1 (car set1))
                         (x2 (car set2))
                      )
                    (cond 
                            ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                            ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                            ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                    )
                 )
             )
    )
)

(define set1 '(1 2 3 5 6 8))
(define set2 '(3 4 6 7 10))
(define set3 '())

(check-equal? (union-set set1 set2) '(1 2 3 4 5 6 7 8 10))
