(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(let ((interval (make-interval 2. 3.)))
    (display (lower-bound interval)) (newline)
    (display (upper-bound interval)) (newline))
