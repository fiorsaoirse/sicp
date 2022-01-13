#lang sicp

(#%require rackunit)

(define (get . arg) '())

(define (put . arg) '())

(define (install-scheme-number-package)
    ; // тут прочие процедуры

    (define (=zero? num)
        (= num 0)
    )

    (put '=zero? '(scheme-number) =zero?)
)

(define (install-rational-package)
    ; // тут прочие процедуры

    (define (numer rational)
        (car rational)
    )

    (define (denom rational)
        (cdr rational)
    )

    (define (=zero? rational)
        (= (numer rational) 0)
    )

    (put '=zero? '(rational) =zero?)
)

(define (install-complex-package)
    ; // тут прочие процедуры

    ; используются процедуры, которые были занесены в диспетчер
    ; при установке пакета комплексных чисел в декартовом и полярном представлении
    (define (=zero? complex)
        (and (= (get 'real-part' complex) 0)
             (= (get 'imag-part' complex) 0)
        )
    )

    (put '=zero? '(complex) =zero?)
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

(define (=zero? num)
    (apply-generic '=zero? num)
)