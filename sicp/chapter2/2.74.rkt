#lang sicp

(#%require rackunit)

(define (get . arg) '())

(define (put . arg) '())

; чтобы убрать различия, необходимо завернуть каждое представление в пакет
; и занести в таблицу диспетчеризации

; пусть таблица имеет вид (операция) (отдел) => где (операция) - диспетчеризация по операции
; (отдел) - диспетчеризация по типу внутри выбранной операции

; (put operation-name department-name procedure) => положим procedure по адресу [тип операции][название-отдела]
; (get operation-name department-name) => возвращает procedure

(define (install-finance-department-package)
    ; внутренние процедуры
    ; помечает запись (record) меткой отдела, чтобы по ней можно было
    ; произвести диспетчеризацию (отдел = тип)
    (define (tag data) (cons 'finance data))

    (define (make-employee name salary address)
        ; т.к. сотрудники создаются в рамках конкретного отдела, помечаются
        ; они во внутренней процедуре
        (tag (list name address salary))
    )

    (define (get-name record)
        (car record)
    )

    (define (get-address record)
        (cadr record)
    )

    (define (get-salary record)
        (caddr record)
    )

    (define (get-record-by-name name records)
        (if (null? records)
            (error "Запись не найдена")
            (let (
                    (current-record (car records))
                )

             (if (eq? (get-name current-record) name)
                 current-record
                 (get-record-by-name name (cdr records))
             )
            )
        )
    )

    ; общие процедуры, интерфейс к основной системе
    ; благодаря тому, что данные "помечаются" именем новой операции,
    ; необходимо лишь класть запись, соответствующую интерфейсу
    ; (put ОБОБЩЕННОЕ-ИМЯ-ОПЕРАЦИИ КОНКРЕТНЫЙ-ОТДЕЛ procedure)
    (put 'get-salary 'finance get-salary)
    (put 'get-record 'finance get-record-by-name)
    'done
)

; аналогичным образом устанавливаем пакет "менеджмент",
; который внутри может иметь иную реализацию и структуру процедур
; (в т.ч. иметь какие-то "ненужные данные" для других отделов,
; типа любимого цвета в работнике)
(define (install-management-department-package)
    ; внутренние процедуры
    (define (mark empl) (cons 'management empl))

    (define (make-super-manager name fav-color salary)
        ; аналогично, раз сотрудники создаются в рамках отдела
        ; они должны соответствовать общему интерфейсу для "помеченных" данных
        (mark (cons fav-color (cons name salary)))
    )

    (define (who-is record)
        (cadr record)
    )

    (define (get-fav-color record)
        (car record)
    )

    (define (how-much record)
        (caddr record)
    )

    (define (find-record name records)
        (if (null? records)
            (error "Запись не найдена")
            (let (
                    (current-record (car records))
                )

             (if (eq? (who-is current-record) name)
                 current-record
                 (find-record name (cdr records))
             )
            )
        )
    )

    ; общие процедуры, интерфейс к основной системе
    ; благодаря тому, что данные "помечаются" именем новой операции,
    ; необходимо лишь класть запись, соответствующую интерфейсу
    ; (put ОБОБЩЕННОЕ-ИМЯ-ОПЕРАЦИИ КОНКРЕТНЫЙ-ОТДЕЛ procedure)
    (put 'get-salary 'management get-salary)
    (put 'get-record 'management find-record)
    'done
)

; обобщенные операции будут выглядеть следующим образом

(define (get-department typed-record)
    (car typed-record)
)

(define (get-content typed-record)
    (cdr typed-record)
)

; чтобы получить из таблицы вида [тип операции][тип департамента] => процедура
; саму процедуру, необходимо 
; 1. получить тип департамента у текущей записи
; 2. получить процедуру по [тип операции][тип департамента]

(define (get-record record dep-employees)
    (let ((dep (get-department record)))
        (let ((proc (get 'get-record dep)))
            (if proc
                (proc (get-content record) dep-employees)
                (error "Нет процедуры")
            )
        )
    )
)

(define (get-salary record)
    (let ((dep (get-department record)))
        (let ((proc (get 'get-salary dep)))
            (if proc
                (proc (get-content record))
                (error "Нет процедуры")
            )
        )
    )
)

(define (find-employee-record record departments)
    (if (null? departments)
        (error "Сотрудник не найден")
        (let (
                (curr-department (car departments))
             )

             (let (
                    (curr-dep-empl (car curr-department))
                    (record-department (get-department record))
                  )
                  ; если нашли нужный департамент, применяем процедуру
                  ; примечание - по-хорошему департамент тоже необходимо маркировать
                  ; меткой, чтобы не выдирать ее из первого сотрудника в списке
                  ; т.к. тут сейчас не продуман кейс со случаем путого списка сотрудников
                  (if (eq? (get-department curr-dep-empl) record-department)
                      ; если департамент совпадает, вызываем процедуру get-record
                      (get-record record curr-department)
                      ; если департамент иной, даже не смотрим, сразу ищем в другом департаменте
                      (find-employee-record record (cdr departments))
                  )
             )
        
        )
    )   
)

; новые изменения вносятся путем инсталляции новых пакетов с соблюдением
; общих интерфесов