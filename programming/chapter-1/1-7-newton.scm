(define (goodEnough? x guess)
    (< (abs (- x guess)) 0.0001)))

(define (average x y)
    (/ (+ x y) 2))

(define (improve x guess)
    (average guess (/ x guess)))

(define (sqrt-itr x guess)
    (if (goodEnough? x (* guess guess))
        guess
        (sqrt-itr x (improve x guess))))

(define (sqrt x)
    (sqrt-itr x 1.0))

(display (sqrt 1)) (newline)
(display (sqrt 2)) (newline)
(display (sqrt 4)) (newline)
(display (sqrt 9)) (newline)
(display (sqrt (+ 4 5))) (newline)
(newline)
