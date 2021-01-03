(define (even? x) (= 0 (modulo x 2)))
(define (square x) (* x x))

(define (fast-exp b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-exp b (/ n 2))))
          (else (* b (fast-exp b (- n 1)))))
)

(display (fast-exp 3 0)) (newline)
(display (fast-exp 3 1)) (newline)
(display (fast-exp 3 2)) (newline)
(display (fast-exp 3 3)) (newline)
(display (fast-exp 3 4)) (newline)
(display (fast-exp 3 101)) (newline)
