#lang sicp

(#%require rackunit)

(define (square x) (* x x))

; метки и способы извлечения тегов/помеченных данных
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Некорректные помеченные данные -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Некорректные помеченные данные -- CONTENTS" datum)))

; диспетчер [тип операции][тип аргументов] => процедура

(define *op-table* 
    ;(make-hash)
    '()
)

(define (put op type proc)
  ;(hash-set! *op-table* (list op type) proc)
  '()
)

(define (get op type)
  ;(hash-ref *op-table* (list op type) #f)
  '()
)

; пакет для работы с обычными числами
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))

  (put 'add '(scheme-number scheme-number)
      (lambda (x y) (tag (+ x y))))

  (put 'sub '(scheme-number scheme-number)
      (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number
    (lambda (x) (tag x)))
'done)

; пакет с рациональными числами
(define (install-rational-package)
  ; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (make-rat n d)
      (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))

  (define (add-rat x y)
      (make-rat (+  (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))
  (define (sub-rat x y)
      (make-rat (-  (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                (* (denom x) (denom y))))

  (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
  (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))

  ; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))

  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  
  ; для ВТОРОГО варианта решения задачи
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
'done)

(define (make-rational n d)
    ((get 'make 'rational) n d)
)

; для ВТОРОГО варианта решения задачи
(define (numer x) (apply-generic 'numer x))

(define (denom x) (apply-generic 'denom x))

; пакет с действительными числами
(define (install-real-package)
  ; внутренние процедуры
  
  ; ... пакета нет, моки

  (define (make-real-inner a b)
      ; ...
      '()
  )

  (define (add-real x y)
      (make-real-inner '() '())
  )

  (define (sub-real x y)
      (make-real-inner '() '())
  )

  (define (mul-real x y)
        (make-real-inner '() '()))

  (define (div-real x y)
        (make-real-inner '() '()))

  ; интерфейс к остальной системе
  (define (tag x) (attach-tag 'real x))

  (put 'add '(real real) (lambda (x y) (tag (add-real x y))))
  (put 'sub '(real real) (lambda (x y) (tag (sub-real x y))))
  (put 'mul '(real real) (lambda (x y) (tag (mul-real x y))))
  (put 'div '(real real) (lambda (x y) (tag (div-real x y))))
  (put 'make 'real (lambda (a b) (tag (make-real-inner a b))))
'done)

(define (make-real a b)
    ((get 'make 'real) a b))

; пакет с декартовыми и полярными числами
(define (install-rectangular-package)
  ; внутренние процедуры
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
      (sqrt (+  (square (real-part z))
                (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a))))

  ; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
          (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
          (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (install-polar-package)
  ; внутренние процедуры
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
      (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
      (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
            (cons (sqrt (+ (square x) (square y)))
                  (atan y x)))

  ; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
      (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
      (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

; комплексные числа
(define (install-complex-package)
  ; процедуры, импортируемые из декартова
  ; и полярного пакетов
  (install-rectangular-package)
  (install-polar-package)

  ; внутренние процедуры
  (define (add-complex z1 z2)
          (make-from-real-imag  (+ (real-part z1) (real-part z2))
                                (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
          (make-from-real-imag  (- (real-part z1) (real-part z2))
                                (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
          (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                              (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
          (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                              (- (angle z1) (angle z2))))

  ; интерфейс к остальной системе
    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
'done)

(define (make-scheme-number d)
    ((get 'make 'scheme-number) d)
)

(define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)
)

(define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)
)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y)
)

(define (apply-generic op . args)
    ; ... опускаем реализацию
    '()
)

; ==== ЗАДАЧА

; ПЕРВЫЙ вариант - добавить процедуру преобразования непосредственно
; в арифметический пакет, ИЗ которого будем преобразовывать, чтобы
; было возможно использовать внутренние селекторы и процедуры

;; обобщенная процедура
; (define (raise x) (apply-generic 'raise x)) 
  
;; добавляем в scheme-number пакет 
;  (put 'raise 'scheme-number  
;           (lambda (x) (make-rational x 1))) 
  
;; добавляем в rational пакет 
;  (put 'raise 'rational 
;           (lambda (x) (make-real (/ (numer x) (denom x))))) 
  
;; добавляем в rational real
;  (put 'raise 'real 
;           (lambda (x) (make-from-real-imag x 0))) 

; ВТОРОЙ вариант:
; пусть преобразованиями занмиается отдельный диспетчер
; т.к. задача "поднятия" типа в общем-то не относится к зоне ответственности 
; арфиметического пакета
; ОДНАКО есть недостаток - нужно определять глобальные селекторы для некоторых чисел

(define *coercion-table*
    ;(make-hash)
    '()
)

; по типу [тип] будет расположена процедура, которая будет преобразовывать аргумент
; из подтипа в надтип
(define (put-coercion type proc)
   ;(hash-set! *coercion-table* (list type1 type2) proc)
   '()
)

; по типу [тип] возвращается процедура, преобразовывающая подтип в надтип (аргумент)
; или false, если данное преобразование невозможно
(define (get-coercion type)
   ;(hash-ref *coercion-table* (list type1 type2) #f)
   '()
)

; установка преобразований
(define (install-coercion-package)

    (define (scheme-number->rational n)
        (make-rational (contents n) 1)
    )

    ; основная проблема вот тут - нужно делать селекторы глобальными
    (define (rational->real r)
       (make-real (/ (numer r) (denom r)))
    )

    (define (real->complex x) 
       (make-complex-from-real-imag x 0)
    )         

    (put-coercion 'scheme-number scheme-number->rational)
    (put-coercion 'rational rational->real)
    (put-coercion 'real real->complex)

    'done
)

; обобщенная процедура raise для варианта 2

(define (raise number)
    (let ((type (type-tag number)))
         (let ((proc (get-coercion type)))
            (if proc
                (proc (contents number))
                (error "Нельзя преобразовать в надтип!")
            )
         )
    )
)
