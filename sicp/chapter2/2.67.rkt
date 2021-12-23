#lang sicp

(#%require rackunit)

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(define (make-leaf symbol weight)
    (list 'leaf symbol weight)
)

(define (leaf? tree)
    (eq? (car tree) 'leaf)
)

(define (get-symbol x)
    (cadr x)
)

(define (get-weight x)
    (caddr x)
)

(define (make-code-tree left-branch right-branch)
    (let (
          (left-symbols (symbols left-branch))
          (right-symbols (symbols right-branch))
          (left-weight (weight left-branch))
          (right-weight (weight right-branch))
        )        

        (list left-branch
              right-branch
              (append left-symbols right-symbols)
              (+ left-weight right-weight))
    )
)

(define (get-left-branch tree)
    (car tree)
)

(define (get-right-branch tree)
    (cadr tree)
)

(define (symbols tree)
    (if (leaf? tree)
        ; если это листовой элемент возвращаем список, состоящий из 1 символа
        ; (список - чтобы списки можно было сложить)
        (list (get-symbol tree))
        ; иначе дерево является набором листьев (уже посчитанным) - "дерево общего вида"
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        (get-weight tree)
        (cadddr tree)
    )
)

; верхняя абстракция
(define (decode bits tree)
    (define (get-next-branch bit tree)
        (cond ((= bit 0) (get-left-branch tree))
              ((= bit 1) (get-right-branch tree))
              (else (error "Бит может быть равен только 1 или 0, а это плохой бит!"))
        )
    )

    (define (decode-inner bits current-branch)
        (if (null? bits)
            '()
            (let (
                    (next-branch (get-next-branch (car bits) current-branch))
                 )
                (if (leaf? next-branch)
                    ; если символ листовой, тогда добавляем символ в результат,
                    ; далее обрабатываем оставшиеся биты
                    (cons (get-symbol next-branch)
                          (decode-inner (cdr bits) tree)
                    )
                    ; или идем дальше по веткам до тех пор, пока не найдем символ
                    (decode-inner (cdr bits) next-branch)
                )
            )
        )
    )

    (decode-inner bits tree)
)

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set))
                      (adjoin-set x (cdr set))
          )
    )
)

(define (make-list-set pairs)
    (if (null? pairs)
        '()
        (let ((current-pair (car pairs)))
            ; берем первую пару, делаем из нее лист "символ-вес" и кладем в список
            ; к уже обработанным парам (предыдущим, помним, что append тут у нас)
            ; вычисляет результат с конца!
            (adjoin-set (make-leaf (car current-pair) (cdr current-pair))
                        (make-list-set (cdr pairs))
            )
        )
    )
)

(define sample-tree 
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree 
                            (make-leaf 'B 2)
                            (make-code-tree
                                (make-leaf 'D 1)
                                (make-leaf 'C 1)
                            )
                    )
    )
)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; должно быть сообщение ADABBCA

(check-equal? (decode sample-message sample-tree) '(A D A B B C A))