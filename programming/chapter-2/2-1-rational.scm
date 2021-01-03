(define (gcd x y)
    (if (= y 0)
         x
         (gcd y (modulo x y))))

(define (make-rat num den)
    (let ((g (gcd num den)))
        (cons
            (/ num g)
            (/ den g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))

(print-rat (make-rat 1 2))
(print-rat (make-rat 50 100))
(print-rat (make-rat 3 9))
(print-rat (make-rat 3 -9))
(print-rat (make-rat -3 9))
(print-rat (make-rat -3 -9))
(print-rat (make-rat 9 3))
(print-rat (make-rat 9 -3))
(print-rat (make-rat -9 3))
(print-rat (make-rat -9 -3))

