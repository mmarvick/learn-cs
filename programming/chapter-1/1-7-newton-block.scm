; scheme --quiet < 1-7-newton-block.scm 

(define (average x y)
        (/ (+ x y) 2))

(define (sqrt x)
    (define (goodEnough? guess)
        (< (abs (- x guess)) 0.0000000001))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (sqrt-itr guess)
        (if (goodEnough? (* guess guess))
            guess
            (sqrt-itr (improve guess))))
    (sqrt-itr 1.0))

(display (sqrt 1)) (newline)
(display (sqrt 2)) (newline)
(display (sqrt 4)) (newline)
(display (sqrt 9)) (newline)
(display (sqrt (+ 4 5))) (newline)
(newline)
