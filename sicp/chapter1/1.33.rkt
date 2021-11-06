#lang sicp

(#%require rackunit)
(#%require racket/trace)

; аккумулирование с фильтрацией значений в интервале от a до b

; рекурсивный вариант

(define (filter-accumulate-rec predicate? combiner null-value a b term next)
    (cond ((> a b) null-value)
          ((predicate? a) (combiner 
                            (term a)
                            (filter-accumulate-rec predicate? combiner null-value (next a) b term next)
                          )
          )
          (else (filter-accumulate-rec predicate? combiner null-value (next a) b term next))
    )
)

; итеративный вариант

(define (filter-accumulate-iter predicate? combiner null-value a b term next)
    (define (iter curr result)
        (cond ((> curr b) result)
              ((predicate? curr) (iter (next curr) (combiner (term curr) result))) 
              (else (iter (next curr) result))
        )
    )

    (iter a null-value)
)

(define (sum x y) (+ x y))

(define (product x y) (* x y))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (even? n)
    (= (remainder n 2) 0)
)

(define (prime? x)
    (define (inner curr)
        (cond ((>= curr x) #t)
              ((= (remainder x curr) 0) #f)
              (else (inner (inc curr)))
        )
    )
    
    (if (= x 1) #f
        (inner 2)
    )
)


(check-equal? (filter-accumulate-rec even? sum 0 10 20 identity inc) 90)
(check-equal? (filter-accumulate-iter even? sum 0 10 20 identity inc) 90)

(check-equal? (filter-accumulate-rec prime? sum 0 1 4 square inc) 13)
(check-equal? (filter-accumulate-rec prime? sum 0 1 20 square inc) 1027)

(check-equal? (filter-accumulate-iter prime? sum 0 1 4 square inc) 13)
(check-equal? (filter-accumulate-iter prime? sum 0 1 20 square inc) 1027)
