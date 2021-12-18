#lang sicp

(#%require rackunit)

; нижний уровень абстракции - конструктор, селекторы, предикаты

(define (variable? x) (symbol? x))

(define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y))
)

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

(define (make-sum x y)
    (cond 
        ((=number? x 0) y)
        ((=number? y 0) x)
        ((and (number? x) (number? y)) (+ x y))
        (else (list x '+ y))
    )
)

(define (make-product x y)
    (cond 
        ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list x '* y))
    )
)

; знак - второй в списке
(define (sum? x)
    (and (pair? x) (eq? (cadr x) '+))
)

(define (addend s)
    (car s)
)

(define (augend s)
    (caddr s)
)

; знак - второй в списке
(define (product? x)
    (and (pair? x) (eq? (cadr x) '*))
)

(define (multiplier p)
    (car p)
)

(define (multiplicand p)
    (caddr p)
)

; второй уровень абстракции - клиент селекторов, предикатов 

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
         (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(check-equal? (deriv '(x + 3) 'x) 1)