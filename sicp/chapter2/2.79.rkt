#lang sicp

(#%require rackunit)

(define (get . arg) '())

(define (put . arg) '())

(define (install-scheme-number-package)
    ; // тут прочие процедуры

    (define (equ? num1 num2)
        (= num1 num2)
    )

    (put 'equ? '(scheme-number scheme-number) equ?)
)

(define (install-rational-package)
    ; // тут прочие процедуры

    (define (numer rational)
        (car rational)
    )

    (define (denom rational)
        (cdr rational)
    )

    (define (equ? rat1 rat2)
        (and (= (numer rat1) (numer rat2))
             (= (denom rat1) (denom rat2))
        )
    )

    (put 'equ? '(rational rational) equ?)
)

(define (install-complex-package)
    ; // тут прочие процедуры

    ; используются процедуры, которые были занесены в диспетчер
    ; при установке пакета комплексных чисел в декартовом и полярном представлении
    (define (equ? complex1 complex2)
        (and (= (get 'real-part' complex1) (get 'real-part' complex2))
             (= (get 'imag-part' complex1) (get 'imag-part' complex2))
        )
    )

    (put 'equ? '(complex complex) equ?)
)

(define (attach-tag type-tag contents)
  (cons type-tag contents)
)


(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Некорректные помеченные данные -- TYPE-TAG" datum))
)

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Некорректные помеченные данные -- CONTENTS" datum))
)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "Нет метода для такого набора типов для операции")
            )
        )
    )
)

(define (equ? n1 n2)
    (apply-generic 'equ? n1 n2)
)