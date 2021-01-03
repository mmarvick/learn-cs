(define (iterative-improve good-enough? improve)
    (define (iter prev next)
        (if (good-enough? prev next)
            next
            (iter next (improve next))))
    (lambda (guess) (iter guess (improve guess))))

(define (sqrt x)
    ((iterative-improve (lambda (prev next) (< (abs (- next prev)) 0.00001))
                        (lambda (guess) (/ (+ guess (/ x guess)) 2))
    ) 1))

(display (sqrt 4.0))(newline)
(display (sqrt 10.0))(newline)
