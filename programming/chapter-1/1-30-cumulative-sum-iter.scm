(load-option 'format)

(define (identity x) x)
(define (cube x) (* x x x))

; switch this between the recursive and iterative process as desired
(define (sum-series a b next calculate)
    (sum-series-iter a b next calculate))

(define (sum-series-recurs a b next calculate)
    (if (> a b) 0
        (+ (calculate a) (sum-series (next a) b next calculate)))
)

(define (sum-series-iter a b next calculate)
    (define (sum-series-step accum a b next calculate)
        (if (> a b) accum
            (sum-series-step (+ accum (calculate a)) (next a) b next calculate)))
    (sum-series-step 0 a b next calculate)
)

(define (sum-ints max)
    (define (next x) (+ x 1))
    (sum-series 1 max next identity)
)

(define (sum-cubes max)
    (define (next x) (+ x 1))
    (sum-series 1 max next cube)
)

(define (pi-estimate max)
    (define (next x) (+ x 4))
    (define (calculate x) (/ 1.0 (* x (+ x 2))))
    (* 8 (sum-series 1 max next calculate))
)

(define (integral func a b dx)
    (define (next x) (+ x dx))
    (define (calculate x) (func (+ x (/ dx 2))))
    (* dx (sum-series a b next calculate))
)

(display "sum ints") (newline)
(display (sum-ints 1)) (newline)
(display (sum-ints 2)) (newline)
(display (sum-ints 3)) (newline)
(display (sum-ints 4)) (newline)
(display (sum-ints 5)) (newline)

(display "sum cubes") (newline)
(display (sum-cubes 1)) (newline)
(display (sum-cubes 2)) (newline)
(display (sum-cubes 3)) (newline)
(display (sum-cubes 4)) (newline)
(display (sum-cubes 5)) (newline)

(display "pi-estimate") (newline)
(display (pi-estimate 1)) (newline)
(display (pi-estimate 2)) (newline)
(display (pi-estimate 3)) (newline)
(display (pi-estimate 4)) (newline)
(display (pi-estimate 5)) (newline)
(display (pi-estimate 100000)) (newline)

(display "cube integrate") (newline)
(display (integral cube 0 1 0.1)) (newline)
(display (integral cube 0 1 0.01)) (newline)
(display (integral cube 0 1 0.001)) (newline)
(display (integral cube 0 1 0.0001)) (newline)
(display (integral cube 0 1 0.000001)) (newline)