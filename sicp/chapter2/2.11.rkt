#lang sicp

(#%require rackunit)

(define (make-interval a b)
    (cons a b)
)

(define (lower-bound interval)
    (car interval)
)

(define (upper-bound interval)
    (cdr interval)
)

(define (mul-interval x y) 
    (let ((x-lower (lower-bound x)) 
          (x-upper (upper-bound x)) 
          (y-lower (lower-bound y)) 
          (y-upper (upper-bound y))) 
      (cond ((>= x-lower 0) 
             (cond ((>= y-lower 0) 
                    (make-interval (* x-lower y-lower) (* x-upper y-upper))) 
                   ((<= y-upper 0) 
                    (make-interval (* x-upper y-lower) (* x-lower y-upper))) 
                   (else 
                    (make-interval (* x-upper y-lower) (* x-upper y-upper))))) 
            ((<= x-upper 0) 
             (cond ((>= y-lower 0) 
                    (make-interval (* x-lower y-upper) (* x-upper y-lower))) 
                   ((<= y-upper 0) 
                    (make-interval (* x-upper y-upper) (* x-lower y-lower))) 
                   (else 
                    (make-interval (* x-lower y-upper) (* x-lower y-lower))))) 
            (else 
             (cond ((>= y-lower 0) 
                    (make-interval (* x-lower y-upper) (* x-upper y-upper))) 
                   ((<= y-upper 0) 
                    (make-interval (* x-upper y-lower) (* x-lower y-lower))) 
                   (else 
                    (make-interval (min (* x-lower y-upper) (* x-upper y-lower)) 
                                   (max (* x-lower y-lower) (* x-upper y-upper)))
                    )
            )
        )
    )
))

