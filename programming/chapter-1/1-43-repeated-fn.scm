(define (square x) (* x x))
(define (compose f g) (lambda (x) (f (g x))))

(define (repeated f times)
    (if (= times 1) f
        (compose (repeated f (- times 1)) f)))

(display ((repeated square 2) 5)) (newline)
