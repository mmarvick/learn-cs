(define (average x y) (/ (+ x y) 2))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))
(define (print-point point)
    (display "(")
    (display (x-point point))
    (display ", ")
    (display (y-point point))
    (display ")")
    (newline))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))
(define (midpoint-segment segment)
    (let ((p1 (start-point segment))
          (p2 (end-point segment)))
            (make-point
                (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2)))))

(print-point (midpoint-segment
    (make-segment
        (make-point 3 5)
        (make-point -1 12))))