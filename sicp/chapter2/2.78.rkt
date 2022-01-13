#lang sicp

(#%require rackunit)

(define (attach-tag tag content)
    (if (number? content)
        content
        (cons tag content)
    )
)

(define (type-tag datum)
    (cond ((number? datum) datum)
          ((pair? datum) (car datum))
          (else (error "НЕКОРРЕКТНО ПОМЕЧЕННЫЕ ДАННЫЕ"))
    )
)

(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "НЕКОРРЕКТНО ПОМЕЧЕННЫЕ ДАННЫЕ"))
    )
)