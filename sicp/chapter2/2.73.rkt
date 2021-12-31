#lang sicp

(#%require rackunit)

(define (get . arg) '())

(define (put . arg) '())

(define (variable? x) (symbol? x))

(define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y))
)

(define (=number? exp num)
    (and (number? exp) (= exp num))
)

; второй уровень абстракции - клиент селекторов, предикатов 

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          ; иначе - по оператору получим метод, которому
          ; в качестве параметров передаем операнды и переменную
          (else ((get 'deriv (operator exp))
                (operands exp)
                var))
    )
)

; а - предикаты включить нельзя, т.к. оба они могут возвращать 2 значения 
; (истина или ложь), т.е. в словаре такой предикат должен иметь ключ "истина"
; или ключ "ложь"

; ну и для определения ему надо передать выражение, в то время как операция сама
; по себе служит ключом

(define (install-sum-package)
    ; внутренние процедуры
    (define (make-sum x y)
        (cond 
            ((=number? x 0) y)
            ((=number? y 0) x)
            ((and (number? x) (number? y)) (+ x y))
            (else (list '+ x y))
        )
    )

    (define (addend exp)
        (car exp)
    )

    (define (augend exp)
        (cdr exp)
    )

    ; интерфейс к остальной системе

    (put 'deriv '(+) (lambda (operands var)
                        (make-sum 
                            (deriv (addend operands) var)
                            (deriv (augend operands) var)
                        )
                     )
    )
)

(define (install-product-package)
    ; внутренние процедуры
    (define (make-product x y)
        (cond 
            ((or (=number? x 0) (=number? y 0)) 0)
            ((=number? x 1) y)
            ((=number? y 1) x)
            ((and (number? x) (number? y)) (* x y))
            (else (list '* x y))
        )
    )

    (define (multiplier exp)
        (car exp)
    )

    (define (multiplicand exp)
        (cdr exp)
    )

    ; интерфейс к остальной системе
    (put 'deriv '(*) (lambda (operands var)
                        (let (
                                (inner-multiplier (make-product 
                                                        (multiplier exp)
                                                        (deriv (multiplicand exp) var)))

                                (inner-multiplicand (make-product
                                                            (deriv (multiplier exp) var)
                                                            (multiplicand exp)))
                                (sum-op (get 'deriv '+))
                              )
                                 ; вызов make-sum
                                 (sum-op inner-multiplier inner-multiplicand)
                            )
                        )
    )
)

(define (install-exp-package)
    ; внутренние процедуры
    (define (make-exponentiation b e)
        (cond 
            ((=number? e 0) 1)
            ((=number? e 1) b)
            ((and (number? b) (number? e)) (power b e))
            (else (list '** b e))
        )
    )

    (define (base exp)
        (car exp)
    )

    (define (exponent exp)
        (cdr exp)
    )

    (define (power base exp)
        (exp (* base (log exp)))
    )

    ; интерфейс к остальной системе
    (put 'deriv '(**) (lambda (operands var)
                        (let (
                                (n (exponent exp))
                                (u (make-exponentiation (base exp) (- (exponent exp) 1)))
                                (du-dx (deriv (base exp) var))
                                (product-op (get 'deriv '*))
                            )

                            (product-op 
                                (product-op n u)
                                du-dx
                            )
                        )
                      )
    )
)

