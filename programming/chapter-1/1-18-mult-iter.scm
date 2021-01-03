(define (even? x) (= (modulo x 2) 0))
(define (half x) (/ x 2))
(define (double x) (* x 2))

(define (fast-mult a b)
    (fast-mult-iter 0 a b)
)

(define (fast-mult-iter accum base times)
    (cond ((= times 0) accum)
          ((even? times) (fast-mult-iter accum (double base) (half times)))
          (else (fast-mult-iter (+ accum base) base (- times 1)))
    )
)

(display (fast-mult 3 0)) (newline)
(display (fast-mult 3 1)) (newline)
(display (fast-mult 5 8)) (newline)
(display (fast-mult 10 10)) (newline)
(display (fast-mult 10 10000001)) (newline) ;about 10s if not using efficient methods 
