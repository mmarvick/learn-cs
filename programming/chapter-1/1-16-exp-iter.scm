(define (even? x) (= 0 (modulo x 2)))
(define (square x) (* x x))

(define (fast-exp b n) (fast-exp-iter 1 b n))

(define (fast-exp-iter accum b n)
    (cond ((= n 0) accum)
          ((even? n) (fast-exp-iter accum (square b) (/ n 2)))
          (else (fast-exp-iter (* b accum) b (- n 1))))
)

(display (fast-exp 3 0)) (newline)
(display (fast-exp 3 1)) (newline)
(display (fast-exp 3 2)) (newline)
(display (fast-exp 3 3)) (newline)
(display (fast-exp 3 4)) (newline)
(display (fast-exp 3 101)) (newline)
