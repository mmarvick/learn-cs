(define (divides2? x) (= (modulo x 2) 0))
(define (divides3? x) (= (modulo x 3) 0))

(define (cons x y) (* (expt 2 x) (expt 3 y)))
(define (car z) (if (divides2? z) (+ 1 (car (/ z 2))) 0))
(define (cdr z) (if (divides3? z) (+ 1 (cdr (/ z 3))) 0))

(let ((pair (cons 5028 2729)))
    (display pair) (newline)
    (display (car pair)) (newline)
    (display (cdr pair)) (newline))
