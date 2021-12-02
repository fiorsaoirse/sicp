#lang sicp

(#%require rackunit)

(define x (list 1 2))
(define y (list 3 4))

(define xy (list x y))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) )
  )
)

(define (fringe items)
    (define (iter source result)
      (cond ((null? source) result)
            ((not (pair? source)) (cons source result) )
            (else (let (
                    (x (car source))
                    (y (cdr source))
                   )
                   
                   (iter x (append (fringe y) result))
              )
            )
        )
    )

    (iter items (list))
)

(define result (fringe xy))

(check-equal? result (list 1 2 3 4))