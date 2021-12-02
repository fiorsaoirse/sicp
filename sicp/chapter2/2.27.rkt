#lang sicp

(#%require rackunit)

(define x (list 1 2))
(define y (list 3 4))

(define xy (list x y))

(define (deep-reverse items)
    (define (iter source result)
        (if (null? source)
            result
            (let (
                    (x (car source))
                 )
                
                (if (not (pair? x))
                    (iter (cdr source) (cons x result) )
                    (iter (cdr source) (cons (deep-reverse x) result) )
                )
            )
        )
    )

    (iter items (list))
)

(check-equal? (deep-reverse xy) (list (list 4 3) (list 2 1)) )