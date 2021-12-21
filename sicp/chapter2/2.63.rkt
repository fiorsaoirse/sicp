#lang sicp

(#%require rackunit)

(define (entry tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-tree entry left-branch right-branch)
    (list entry left-branch right-branch)
)

(define (addjoin element tree)
    (cond ((null? tree) (make-tree element '() '()))
          ((< element (entry tree))
                (make-tree
                    (entry tree)
                    (addjoin element (left-branch tree))
                    (right-branch tree)
                )
          )
          ((> element (entry tree))
                (make-tree
                    (entry tree)
                    (left-branch tree)
                    (addjoin element (right-branch tree))
                )
          )
    )
)

; т.к. множество представлено деревом,
; вычисление, является ли искомое элементом списка, будет иметь следующий вид
(define (element-of-set? element set)
    (cond ((null? set) #f)
          ((= element (entry set)) #t)
          ((< element (entry set))
                (element-of-set? element (left-branch set))
          )
          ((> element (entry set))
                (element-of-set? element (right-branch set))
          )
    )
)

; util
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2) )
  )
)

; задача

(define (tree->list1 tree)
    (if (null? tree)
        '()
        ; т.е. тут левая часть преобразуется в список, который потом append-ится
        ; к списку, состоящему из ( входного элемента и преобразованному списку из правой части) 
        ; функция порождает линейно-рекурсивный процесс

        ; на каждом шаге рекурсии (для каждого узла дерева) вызывается процедура append,
        ; которая на самом деле не выполняется за константное время, а требует числа шагов 
        ; порядка количества элементов в поддереве. Будем предполагать, что дерево сбалансировано,
        ; то есть в поддеревьях каждого узла примерно поровну элементов.
        ; Пусть некоторое дерево содержит n элементов. Тогда поддеревья его корневого элемента содержат
        ; по n/2 элементов. Процедура tree->list-1 на корневом элементе потребует n/2 шагов для append 
        ; и запустится на каждом из поддеревьев. На каждом из них (их два) аналогичным образом потребуется
        ; по n/4 шагов для append, то есть суммарно опять n/2. И так далее.
        ; То есть, спускаясь на один уровень по дереву от корня к листовым элементам, мы выполняем
        ; для всех вершин на этом уровне в сумме около n/2 шагов. Уровней в сбалансированном бинароном дереве,
        ; с которым мы имеем дело, примерно двоичный логарифм от числа элементов. 
        ; Таким образом нам потребуется примерно n/2 * log2n, следовательно порядок роста
        ; Θ(n log n).
        (append (tree->list1 (left-branch tree))
                (cons (entry tree)
                      (tree->list1 (right-branch tree))
                )        
        )
    )
)

(define (tree->list2 tree)
    (define (copy-to-list tree result-tree)
        (if (null? tree)
            result-tree
            ; тут метод сначала собирает правую часть дерева и составляет список (1)
            ; затем берет этот список (1), ставит его как результирующий и собирает
            ; левую часть дерева

            ; для каждого элемента ф-я вызывает саму себя единожды, поэтому
            ; порядок роста 0(n)
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree) result-tree)
                          )
            )
        )
    )

    (copy-to-list tree '())
)

(define first-tree (make-tree 7 
                        (make-tree 3 
                               (make-tree 1 '() '())
                               (make-tree 5 '() '())
                        )
                        (make-tree 9
                               '()
                               (make-tree 11 '() '())
                        )
                   )
)

(define second-tree (make-tree 3
                            (make-tree 1 '() '())
                            (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                            '()
                                            (make-tree 11 '() '())
                                    )
                            )
                    )

)

(check-equal? (tree->list1 first-tree) '(1 3 5 7 9 11))
(check-equal? (tree->list1 second-tree) '(1 3 5 7 9 11))

(check-equal? (tree->list2 first-tree) '(1 3 5 7 9 11))
(check-equal? (tree->list2 second-tree) '(1 3 5 7 9 11))