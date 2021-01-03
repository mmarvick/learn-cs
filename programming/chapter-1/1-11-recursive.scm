(define (func n)
    (cond
        ((< n 3) n)
        (else (+
            (func (- n 1))
            (* 2 (func (- n 2)))
            (* 3 (func (- n 3)))
        ))
    )
)

(display (func 0)) (newline)
(display (func 1)) (newline)
(display (func 2)) (newline)
(display (func 3)) (newline)
(display (func 4)) (newline)
(display (func 5)) (newline)
(display (func 28)) (newline) ; about 13 seconds

