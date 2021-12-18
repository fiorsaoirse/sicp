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
        (else (list '+ x y))
    )
)

(define (make-product x y)
    (cond 
        ((or (=number? x 0) (=number? y 0)) 0)
        ((=number? x 1) y)
        ((=number? y 1) x)
        ((and (number? x) (number? y)) (* x y))
        (else (list '* x y))
    )
)

(define (sum? x)
    (and (pair? x) (eq? (car x) '+))
)

(define (addend s)
    (cadr s)
)

(define (augend s)
    (caddr s)
)

(define (product? x)
    (and (pair? x) (eq? (car x) '*))
)

(define (multiplier p)
    (cadr p)
)

(define (multiplicand p)
    (caddr p)
)

; задача

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**))
)

(define (base exp)
    (cadr exp)
)

(define (exponent exp)
    (caddr exp)
)

(define (power base exp)
    (exp (* base (log exp)))
)

(define (make-exponentiation b e)
    (cond 
        ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (power b e))
        (else (list '** b e))
    )
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
         ((exponentiation? exp)
            (let (
                    (n (exponent exp))
                    (u (make-exponentiation (base exp) (- (exponent exp) 1)))
                    (du-dx (deriv (base exp) var))
                 )
                (make-product
                    (make-product n u)
                    du-dx
                )
            )
            
            (make-product 
                (* (exponent exp) 
                    (make-exponentiation)
                )
                (deriv (base exp) var)
            )
          )
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(check-equal? (deriv '(+ x 3) 'x) 1)
;(check-equal? (deriv '(+ x y) 'x) 'y)