(define (cons2 x y) (lambda (m) (m x y)))
(define (car2 z) (z (lambda (x y) x)))
(define (cdr2 z) (z (lambda (x y) y)))

(let ((pair (cons2 3 7)))
    (display (car2 pair)) (newline)
    (display (cdr2 pair)) (newline))
