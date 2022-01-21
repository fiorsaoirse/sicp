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

  (put 'raise 'scheme-number
    (lambda (x) (make-rational x 1))
  )
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

   (put 'raise 'rational 
          (lambda (x) (make-real (/ (numer x) (denom x)))))
'done)

(define (make-rational n d)
    ((get 'make 'rational) n d))

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

   (put 'raise 'real 
          (lambda (x) (make-from-real-imag x 0))) 
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

(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

(define (raise type)
    (get 'raise type)
)

; ЗАДАЧА
(define (every? value sequence)
    (if (null? sequence)
        #t
        (let ((first (car sequence)))
             (if (eq? value first)
                 (every? value (cdr sequence))
                 #f
             )
        )
    )
)

(define (filter predicate sequence)
    (if (null? sequence)
        '()
        (let ((head (car sequence))
              (tail (cdr sequence)))
            (if (predicate head)
                (cons head (filter predicate tail))
                (filter predicate tail)
            )
        )
    )
)

(define tower '(scheme-number rational real complex))

; TODO: ошибка, если только один тип есть в башне
(define (higher? tag1 tag2)
    (define (inner current-tower)
        (if (null? current-tower)
            (error "Данные типы не участвуют в башне типов!")
            (let ((current-type (car current-tower)))
                ; если сначала встретился тег1, значит, тег2 выше
                ; будет работать в т.ч. со случаем, если тег1 равен тегу2
                ; т.к. сначала "встретится" тег1, что исключает необходимость
                ; проверки тегов на эквивалентность
                 (cond ((eq? current-type tag1) #f)
                       ((eq? current-type tag2) #t)
                       (else (inner (cdr current-tower)))
                 )
            )
        )
    )

    (inner tower)
)

; ПРИМЕЧАНИЕ - задачу немного изменила, чтобы была возможность проверять
; наличие процедур для разного списка типов. Последовательно поднимаем все
; аргументы низшего типа к его супертипу и смотрим на наличие процедуры (а не сразу все
; аргументы к наивысшему)

; алгоритм следующий:
; 1) ищем процедуру, которая подходит под текущий список типов.
; если такая процедура есть, идем в п.1.1, если нет - идем в п.1.2
; 1.1) применяем процедуру => выход
; 1.2) ищем низший тип среди всех типов аргументов
; 2) проверяем, можем ли мы поднять текущий низший тип.
; если можем, идем в п.2.2, если не можем - идем в п.2.1
; 2.1) данное выполнение процедуры с аргументами исходных типов невозможно,
; показываем ошибку => выход
; 2.2) поднимаем аргументы низшего типа в его супертип, получаем новый
; список аргументов, возврат на п.1

(define (apply-generic op . args)
    ; процедура, возвращающая тег нижнего подтипа
    (define (get-lowest-type tags)
        (define (inner lowest-tag rest-tags)
            (if (null? rest-tags)
                lowest-tag
                (let ((head-tag (car rest-tags))
                      (tail-tags (cdr rest-tags))
                     )
                        (if (higher? lowest-tag head-tag)
                            ; если нижайший тег выше текущего, текущйи
                            ; становится нижайшим
                            (inner head-tag tail-tags)
                            ; иначе просто идем по башне дальше
                            (inner lowest-tag tail-tags)
                        )
                )
            )
        )

        (inner (car tags) (cdr tags))
    )

    ; coerced-args - список аргументов, по которым будем искать совпадения
    ; в таблице операций
    (define (iter coerced-args)
        (let ((type-tags (map type-tag args)))
            ; пробуем найти процедуру по текущему списку типов
            (let ((proc (get op type-tags)))
                (if proc
                    ; п.1.1
                    (apply proc (map contents args))
                    ; п.1.2
                    (let ((lowest-type (get-lowest-type type-tags)))
                        ; п.2
                        (let ((subtype->supertype (raise lowest-type)))
                             ; п.2.2
                             (if subtype->supertype
                                (let ((updated-args (map (lambda (arg)
                                                            (let ((tag (type-tag arg)))
                                                                (if (eq? lowest-type tag)
                                                                    (subtype->supertype (contents arg))
                                                                    arg
                                                                )
                                                            )
                                                         )
                                                         args)))
                                    (iter updated-args)
                                )
                                ; п.2.1
                                (error "Невозможно перевести аргументы в супертип!")
                             )
                        )
                    )
                )
            )
        )
    )
    
    (iter args)
)
