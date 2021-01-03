; 

(define (func n)
    (cond
        ((< n 3) n)
        (else (func-iter 0 1 2 3 n))
    )
)

(define (func-iter a b c idx n)
    (if (> idx n)
        c
        (func-iter
            b
            c
            (+ (* 3 a) (* 2 b) c)
            (+ idx 1)
            n
        )
    )
)

(display (func 0)) (newline)
(display (func 1)) (newline)
(display (func 2)) (newline)
(display (func 3)) (newline)
(display (func 4)) (newline)
(display (func 5)) (newline)
(display (func 28)) (newline) ; <1 s
(display (func 1000)) (newline); <1 s -- MUCH faster!
