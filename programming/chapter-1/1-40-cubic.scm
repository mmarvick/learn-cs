(define (cubic a b c)
    (lambda (x)
        (+ (* x x x)
           (* a x x)
           (* b x)
           c)))

(define (deriv fn)
    (define dx 0.00001)
    (lambda (x)
        (/
            (- (fn (+ x dx)) (fn x))
            dx
        )))

(define (newton-transform fn)
    (lambda (x)
        (- x
           (/ (fn x)
              ((deriv fn) x)))))

(define (fixed-point fn x)
    (define tolerance 0.000001)
    (define (close-enough? x y)
        (< (abs (- x y)) tolerance))
    (let ((y (fn x)))
        (if (close-enough? x y)
            y
            (fixed-point fn y))))

(define (newtons-method fn guess)
    (fixed-point (newton-transform fn) guess))

(display "zero of function") (newline)
(display (newtons-method (cubic 0 0 0) 1)) (newline)    ; expect 0
(display (newtons-method (cubic 5 -2 1) 1)) (newline)   ; expect -5.40431
(display (newtons-method (cubic 1 5 12) 1)) (newline)   ; expect -1.83617
