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

; задача
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq))
      )
  )
)

(define (filter predicate sequance)
  (if (null? sequance)
      nil
      (let (
              (head (car sequance))
              (tail (cdr sequance))
           )
        (if (predicate head)
            (cons head (filter predicate tail))
            (filter predicate tail)
        )
      )
  )
)

(define (non-number? x)
    (not (number? x))
)

(define (make-sum-list seq) 
  (if (null? (cdr seq)) 
      (car seq) 
      (cons '+ seq))
)

(define (make-sum . terms)
    (let (
            (vars (filter non-number? terms))
            (const (accumulate + 0 (filter number? terms)))
         )

         (cond 
            ((null? vars) const)
            ((= 0 const) (make-sum-list vars))
            (else (make-sum-list (cons const vars)))
         )
    )
)

(define (make-product-list seq) 
  (if (null? (cdr seq)) 
      (car seq) 
      (cons '* seq))
)

(define (make-product . terms)
    (let (
            (vars (filter non-number? terms))
            (const (accumulate * 1 (filter number? terms)))
         )

         (cond 
            ((null? vars) const)
            ((= 0 const) 0)
            ((= 1 const) (make-product-list vars))
            (else (make-product-list (cons const vars)))
         )
    )
)
; ---

(define (sum? x)
    (and (pair? x) (eq? (car x) '+))
)

(define (sum-list seq)
    (let (
            (x (car seq))
            (y (cdr seq))
            (rest (caddr seq))
         )
        (if (null? rest)
            (+ x y)
            (+ x y (sum-list rest))
        )
    )
)

(define (addend s)
    (cadr s)
)

(define (augend s) 
  (make-sum-list (cddr s))
)

(define (product? x)
    (and (pair? x) (eq? (car x) '*))
)

(define (multiplier p)
    (cadr p)
)

(define (multiplicand p)
    (make-product-list (cddr p))
)

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