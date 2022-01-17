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
'done)

(define (make-rational n d)
    ((get 'make 'rational) n d))

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

; диспетчер преобразований типов [из типа][в тип] => процедура

(define *coercion-table*
    ;(make-hash)
    '()
)

(define (put-coercion type1 type2 proc)
   ;(hash-set! *coercion-table* (list type1 type2) proc)
   '()
)

(define (get-coercion type1 type2)
   ;(hash-ref *coercion-table* (list type1 type2) #f)
   '()
)

; установка преобразований
(define (install-coercion-package)
   (define (scheme-number->complex n)
     (make-complex-from-real-imag (contents n) 0))

   (define (scheme-number->rational n)
       (make-rational (contents n) 1))

   (define (complex->rational n) 
       (make-rational 1 2))         

    (put-coercion 'scheme-number 'rational scheme-number->rational)
    (put-coercion 'scheme-number 'complex scheme-number->complex)
    (put-coercion 'complex 'rational complex->rational)

    'done
)

; с помощью accumulate можно создать обобщенные операции
; на неизвестное кол-во аргументов
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq))
      )
  )
)

; обобщенные арифметические процедуры
(define (add . args)
    (accumulate (lambda (current-arg accumulated)
                    (apply-generic 'add (list current-arg accumulated))
                )
                0
                args
    )
)

(define (sub x y . args) 
    (let ((sub-x-y (apply-generic 'sub (list x y))))
        (if (null? args)
            sub-x-y
            (sub sub-x-y (car args) (cdr args))
        )
    )
)

(define (mul . args) 
    (accumulate (lambda (current-arg accumulated)
                    (apply-generic 'mul (list current-arg accumulated))
                )
                1
                args
    )
)

(define (div x y . args) 
    (let ((div-x-y (apply-generic 'div (list x y))))
        (if (null? args)
            div-x-y
            (div div-x-y (car args) (cdr args))
        )
    )
)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

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

(install-coercion-package)
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

(define (apply-generic op . args)
    ; процедура перевода аргумента в целевой тип
    ; возвращает аргумент целевого типа или ложь, если это невозможно
    (define (coerce target-type arg)
        (let ((current-type (type-tag arg)))
            ; если текущий тип и так нужного типа,
            ; его не надо преобразовывать
            (if (eq? target-type current-type)
                ; в таком случае возвращаем тот же аргумент
                arg
                (let ((current->target (get-coercion current-type target-type)))
                    (if current->target
                        (current->target arg)
                        #f
                    )
                )
            )
        )
    )

    ; процедура перевода всех аргументов в целевой тип
    ; возвращает список аргументов целевого типа или ложь, если преобразования невозможны
    (define (coerce-all target-type args)
        (define (coerce-iter raw-args coerced-args)
            (if (null? raw-args)
                coerced-args
                (let ((coerced-arg (coerce target-type (car raw-args))))
                    (if coerced-arg 
                        (coerce-iter (cdr raw-args) (cons coerced-arg coerced-args))
                        #f
                    )
                )
            )
        )

        (coerce-iter args '())
    )

    ; target-types - список типов начального набора аргументов,
    ; в которые мы можем сделать перевод

    ; coerced-args - список аргументов, по которым будем искать совпадения
    ; в таблице операций
    (define (iter coerced-args target-types)
        (let ((type-tags (map type-tag args)))
            ; пробуем найти процедуру по текущему списку типов
            (let ((proc (get op type-tags)))
                (if proc
                    (apply proc (map contents args))
                    ; если не осталось типов, к которому можно попробовать привести
                    ; все аргументы, значит, методов нет
                    (if (null? target-types)
                        (error "Нет методов для данных типов")
                        (let ((next-target-type (car target-types)))
                        ; coercions - список аргументов, приведенных к типу next-target-type
                        ; или false, если все аргументы нельзя привести к данному целевому типу
                        
                        ; в таком случае что должна возвращать get-coercion на попытку
                        ; приведения к тому же типу? предположим, что это будет identity
                        (let (
                                (coercions (coerce-all next-target-type args))
                                (rest-target-tags (cdr target-types))
                             )
                            ; если все типы можно привести
                            (if (not (null? coercions))
                                ; пробуем применить дженерик ко списку новых аргументов,\
                                ; при этом аналогично уменьшаем список переведенных типов
                                (iter coercions rest-target-tags)
                                ; или пробуем привести все аргументы к следующему типу
                                (iter coerced-args rest-target-tags)
                            )
                        )
                        )
                    )
                )
            )
        )
    )
    
    (iter args (map type-tag args))
)


; данная таблица не будет общей для случаев, когда надо перевести в другой тип
; лишь часть аргументов, а не все

; types: A B C 
; registered op: (op some-A some-B some-B) 
; registered coercion: A->B C->B 
; Situation: Evaluating (apply-generic op A B C) will only try (op A B C), (op B B B) and fail  
; while we can just coerce C to B to evaluate (op A B B) instead 