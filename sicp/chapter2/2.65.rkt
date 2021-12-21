#lang sicp

(#%require rackunit)

; первый уровень абстракции - деревья
; используются списки, чтобы построить деревья

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

(define (tree->list tree)
    (define (copy-to-list tree result-tree)
        (if (null? tree)
            result-tree
            (copy-to-list (left-branch tree)
                          (cons (entry tree)
                                (copy-to-list (right-branch tree) result-tree)
                          )
            )
        )
    )

    (copy-to-list tree '())
)

(define (list->tree elements)
    (define (partial-tree elts n)
        (if (= n 0)
            (cons '() elts)
            (let ((left-size (quotient (- n 1) 2)))
                (display "балансируем дерево...")
            )
        )
    )

    (partial-tree elements (length elements))
)

; второй уровень абстракции - множества, используют деревья

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

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let (
                (x1 (car set1))
                (x2 (car set2))
             )

             (cond 
                ((= x1 x2)
                    (cons x1 (intersection-set (cdr set1) (cdr set2)))
                )
                ((< x1 x2)
                    (intersection-set (cdr set1) set2)
                )
                ((> x1 x2)
                    (intersection-set set1 (cdr set2))
                )
             )
        )
    )

    (cond 
         ((or (null? set1) (null? set2)) '())
         ((element-of-set? (car set1) set2)
            (cons (car set1)
                  (intersection-set (cdr set1) set2)
            )
         )
         (else (intersection-set (cdr set1) set2))
    )
)

(define (union-set set1 set2)
    (cond
            ((null? set1) set2)
            ((null? set2) set1)
            (else 
                 (let (
                         (x1 (car set1))
                         (x2 (car set2))
                      )
                    (cond 
                            ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                            ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                            ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                    )
                 )
             )
    )
)

; задача пересечения деревьев сводится к переводу
; каждого дерева в список, нахождение пересечения этих списков
; и построению дерева из найденного пересечения

(define (intersection-set-tree tree1 tree2)
    (list->tree (intersection-set
                    (tree->list tree1)
                    (tree->list tree2)
                )
    )
)

; аналогично для объединения
; перевод каждого дерева в список, нахождение объединения этих списков
; и построению дерева из найденного пересечения
(define (union-set-tree tree1 tree2)
    (list->tree (union-set
                    (tree->list tree1)
                    (tree->list tree2)
                )
    )
)