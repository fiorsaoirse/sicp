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

(define (symbol-leaf x)
    (cadr x)
)

(define (weight-leaf x)
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
        (list (symbol-leaf tree))
        ; иначе дерево является набором листьев (уже посчитанным) - "дерево общего вида"
        (caddr tree)
    )
)

(define (weight tree)
    (if (leaf? tree)
        ; вес самого листа
        (weight-leaf tree)
        ; сумма весов всех листьев
        (cadddr tree)
    )
)

(define (element-of-set? x set)
    (cond ((null? set) #f)
          ((eq? (car set) x) #t)
          (else (element-of-set? x (cdr set)))
    )
)

; верхняя абстракция

(define (encode-symbol symbol tree)
    (define (encode-symbol-inner current-branch bits)
        (if (leaf? current-branch)
            bits
            (let ((left (get-left-branch current-branch))
                  (right (get-right-branch current-branch)))
                  
                    (cond ((element-of-set? symbol (symbols left)) (encode-symbol-inner left (append bits (list 0))))
                          ((element-of-set? symbol (symbols right)) (encode-symbol-inner right (append bits (list 1))))
                          (else (error "нет такого символа"))
                    )
            )
        )
    )
    
    (encode-symbol-inner tree '())
)

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
              (encode (cdr message) tree)
        )
    )
)

(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))
                )
          )
    )
)

(define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((current-pair (car pairs)))
            ; берем первую пару, делаем из нее лист "символ-вес" и кладем в список
            ; к уже обработанным парам (предыдущим, помним, что append тут у нас)
            ; вычисляет результат с конца!
            (adjoin-set (make-leaf (car current-pair) (cadr current-pair))
                        (make-leaf-set (cdr pairs))
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

(define sample-message '(A D A B B C A))

(check-equal? (encode sample-message sample-tree) '(0 1 1 0 0 1 0 1 0 1 1 1 0))