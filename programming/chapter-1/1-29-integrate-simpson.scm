(define (cube x) (* x x x))

; switch this between the recursive and iterative process as desired
(define (sum-series a b next calculate)
    (define (sum-series-step accum a b next calculate)
        (if (> a b) accum
            (sum-series-step (+ accum (calculate a)) (next a) b next calculate)))
    (sum-series-step 0 a b next calculate)
)

(define (integral func a b dx)
    (define (next x) (+ x dx dx))
    (define (calculate x) (+
        (* 4 (func x))
        (* 2 (func (+ x dx)))))
    (*
        (/ dx 3)
        (+
            (func a)
            (sum-series (+ a dx) (- b dx dx) next calculate)
            (* 4 (func (- b dx)))
            (func b)
        )
    )
)

(display "cube integrate") (newline)
(display (integral cube 0 1 0.1)) (newline)
(display (integral cube 0 1 0.01)) (newline)  ; n = 100
(display (integral cube 0 1 0.001)) (newline) ; n = 1000
(display (integral cube 0 1 0.0001)) (newline)
(display (integral cube 0 1 0.000001)) (newline)

; it's better than the other rule, by several orders of magnitude