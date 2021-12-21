#lang sicp

(#%require rackunit)

; нижний слой абстракции
(define (entry tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-key x)
    (list 'key x)
)

(define (make-tree entry left-branch right-branch)
    (list (make-key entry) left-branch right-branch)
)

(define (get-key tree)
    (cadr (entry tree))
)

(define (lookup key tree)
    (cond ((null? tree) #f)
          ((equal? key (get-key tree)) (car tree))
          ((< key (get-key tree)) 
            (lookup key (left-branch tree))
          )
          ((> key (get-key tree))
            (lookup key (right-branch tree))
          )
    )
)

(define test-tree (make-tree 7 
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

(check-equal? (lookup 11 test-tree) (list 'key 11))