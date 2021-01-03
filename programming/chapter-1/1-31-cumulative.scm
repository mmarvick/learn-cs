(define (identity x) x)
(define (increment x) (+ x 1))

(define (product-series a b next calculate)
    (define (product-series-iter accum a b next calculate)
        (if (> a b) accum
            (product-series-iter (* accum (calculate a)) (next a) b next calculate)))
    (product-series-iter 1 a b next calculate))

(define (factorial n)
    (product-series 2 n increment identity))

(define (pi-approx max-den)
    (define (next x) (+ x 2))
    (define (calculate x) (/ (* (- x 1) (+ x 1)) (* x x)))
    (* 4.0 (product-series 3 max-den next calculate))
)

(display "factorial") (newline)
(display (factorial 1)) (newline)
(display (factorial 2)) (newline)
(display (factorial 3)) (newline)
(display (factorial 4)) (newline)
(display (factorial 5)) (newline)

(display "pi approx") (newline)
(display (pi-approx 3)) (newline)
(display (pi-approx 5)) (newline)
(display (pi-approx 7)) (newline)
(display (pi-approx 9)) (newline)
(display (pi-approx 101)) (newline)
(display (pi-approx 10001)) (newline)