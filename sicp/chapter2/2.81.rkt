#lang sicp

(#%require rackunit)

(define (get . arg) '())

(define (put . arg) '())

(define (get-coercion . arg) '())

(define (put-coercion . arg) '())

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

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args 2))
                    (let (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                         )
                         (let (
                                (t1->t2 (get-coercion type1 type2))
                                (t2->t1 (get-coercion type2 type1))
                              )
                              ; если есть операция приведения типа1 в тип2, применяем
                              ; дженерик [тип операции][(перевод-в-тип2 аргумент1)][аргумент2]
                              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                                    (else (error "Нет методов для приведения типов"))
                              )
                         )
                    )
                    (error "Нет метода для этих типов")
                )
            )
        )
    )
)

; задача

; предусловие - определены процедуры, которые переводят тип в сам себя (identity)

; а) если попытаться вызвать процедуру возведения в степень для complex числа,
; процедура get не найдет операции exp для списка типов '(complex complex) и будет
; попытка привести тип complex в тип complex (т.к. они представлены как "разные").будет
; сама процедура перевода типа в тот же тип породит новый вызов этой же процедуры с этими
; же типами и произойдет ровно то же самое -> бесконечная рекурсия значение в строках 49 и 50
; будут истинными, соответственно в первом же условии cond будет повторный вызов этой же
; процедуры 

; б) думаю, что Хьюго не прав, и однотипные аргументы должны обрабатываться тем же пакетом,
; что и устанавливает их. если процедуры, которая позволит обработать два аргумента, нет,
; это значит, ее и не должно быть, или ее забыли добавить

; в)

(define (apply-generic2 op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args 2))
                    (let (
                            (type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args))
                         )
                         (if (eq? type1 type2)
                             (error "Нет процедуры для данных типов")
                             (let (
                                    (t1->t2 (get-coercion type1 type2))
                                    (t2->t1 (get-coercion type2 type1))
                                  )
                              ; если есть операция приведения типа1 в тип2, применяем
                              ; дженерик [тип операции][(перевод-в-тип2 аргумент1)][аргумент2]
                              (cond (t1->t2 (apply-generic2 op (t1->t2 a1) a2))
                                    (t2->t1 (apply-generic2 op a1 (t2->t1 a2)))
                                    (else (error "Нет методов для приведения типов"))
                              )
                         )
                        )
                    )
                    (error "Нет процедуры для данных типов")
                )
            )
        )
    )
)
