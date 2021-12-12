#lang sicp

(#%require rackunit)

(define (split main-op sub-op)
    (define (inner-split painter deep)
        (if (= 0 deep)
            painter
            (let (
                    (smaller-painter (inner-split painter (- deep 1)))
                 )
                (main-op painter (sub-op smaller-painter smaller-painter))
            )
        )
    )

    inner-split
)