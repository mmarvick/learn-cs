(define one (lambda (x) 1))

(define (cont-frac n d k)
    (define (cont-frac-iterative-step k-cur accum)
        (let ((n (n k-cur)) (d (d k-cur)))
            (let ((div (/ n (+ d accum))))
                (if (= k-cur 1) div (cont-frac-iterative-step (- k-cur 1) div)))))
    (cont-frac-iterative-step k 0))

(define (tan x k)
    (define num (lambda (k) (if (= k 1) x (- (expt x k)))))
    (define den (lambda (k) (- (* 2 k) 1)))
    (cont-frac num den k))

; tan 0.3: 0.30933625
; tan 1:   1.55740772
; tan 5:   -3.38051501
(display "tan 0.3") (newline)
(display (tan 0.3 1)) (newline)
(display (tan 0.3 2)) (newline)
(display (tan 0.3 3)) (newline)
(display (tan 0.3 4)) (newline)
(display (tan 0.3 5)) (newline)
(display (tan 0.3 15)) (newline)
(display (tan 0.3 100)) (newline)

(display "tan 1") (newline)
(display (tan 1.0 1)) (newline)
(display (tan 1.0 2)) (newline)
(display (tan 1.0 3)) (newline)
(display (tan 1.0 4)) (newline)
(display (tan 1.0 5)) (newline)
(display (tan 1.0 15)) (newline)
(display (tan 1.0 100)) (newline)

; this doesn't seem - I suspect we need to be in the range of -pi/2 to pi/2
(display "tan 5") (newline)
(display (tan 5.0 1)) (newline)
(display (tan 5.0 2)) (newline)
(display (tan 5.0 3)) (newline)
(display (tan 5.0 4)) (newline)
(display (tan 5.0 5)) (newline)
(display (tan 5.0 15)) (newline)   
(display (tan 5.0 100)) (newline)
