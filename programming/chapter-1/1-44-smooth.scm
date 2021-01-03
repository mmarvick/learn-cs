(define dx 0.0001)
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f times)
    (if (= times 1) f
        (compose (repeated f (- times 1)) f)))

(define (smooth fn)
    (lambda (x)
        (/ (+ (fn (- x dx))
            (fn x)
            (fn (+ x dx)))
        3)))

(define (smooth-n-fold n) (repeated smooth n))

(display ((smooth square) 2)) (newline)
(display (((smooth-n-fold 1) square) 2)) (newline)
(display (((smooth-n-fold 2) square) 2)) (newline)

(display ((smooth inc) 2)) (newline)
(display (((smooth-n-fold 1) inc) 2)) (newline)
(display (((smooth-n-fold 2) inc) 2)) (newline)