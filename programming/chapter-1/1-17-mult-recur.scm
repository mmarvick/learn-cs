(define (even? x) (= (modulo x 2) 0))
(define (half x) (/ x 2))
(define (double x) (* x 2))

(define (fast-mult base times)
    (cond ((= times 0) 0)
          ((even? times) (fast-mult (double base) (half times)))
          (else (+ base (fast-mult base (- times 1))))
    )
)

(display (fast-mult 3 0)) (newline)
(display (fast-mult 3 1)) (newline)
(display (fast-mult 5 8)) (newline)
(display (fast-mult 10 10)) (newline)
(display (fast-mult 10 10000001)) (newline) ;about 10s if not using efficient methods 
