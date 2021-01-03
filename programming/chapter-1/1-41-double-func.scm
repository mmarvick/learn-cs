(define (inc x) (+ x 1))
(define (double fn) (lambda (x) (fn (fn x))))

(display (((double double) inc) 0)) (newline)
(display (((double (double double)) inc) 0)) (newline)              ; this is most like the answer
(display (((double (double (double double))) inc) 0)) (newline)

(display (((double double) inc) 0)) (newline)
(display ((double ((double double) inc)) 0)) (newline)
(display ((double (double ((double double) inc))) 0)) (newline)

(display (((double (double double)) inc) 5)) (newline)              ; this is the problem asked in the book