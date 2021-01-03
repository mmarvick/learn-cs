(define cube (lambda (x) (* x x x)))
(define (derivative fn)
    (define dx 0.000001)
    (lambda (x) (/ (- (fn (+ x dx)) (fn x)) dx)))
(define (derivative-wrap fn x)
    (define dx 0.000001)
    (/ (- (fn (+ x dx)) (fn x)) dx))

; get the derivative of a cube
; first: take a function, and return a function that gets the derivative of the passed function with param x
; second: take a function and x, and evaluate the derivative of the passed function at that x
(define d-cube (derivative cube))
(define d-cube-2 (lambda (x) (derivative-wrap cube x)))
(display (d-cube 0.5)) (newline)
(display (d-cube-2 0.5)) (newline)

; get a function that 6x's things, by using a 2x and 3x function
; first: create functions that double and triple x, and write a function that wraps the calls
; second: create function that takes a function and returns a new function, that 3x's the
;         results from the passed function. Then just call that directly
(define double (lambda (x) (* 2 x)))
(define double-fn (lambda (fn) (lambda (x) (* 2 (fn x)))))
(define triple (lambda (x) (* 3 x)))

(define sextuple-1 (lambda (x) (double (triple x))))
(define sextuple-2 (lambda (x) ((double-fn triple) x)))

(display (sextuple-1 2.0)) (newline)
(display (sextuple-2 2.0)) (newline)