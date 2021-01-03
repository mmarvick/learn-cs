(define average (lambda (x y) (/ (+ x y) 2)))

(define (fixed-point func initial-guess tolerance)
    (define (close-enough? a b) (< (abs (- a b)) tolerance))
    (define (print-out index guess) (display index) (display ": ") (display guess) (newline))
    (define (try guess-input count)
        (let ((guess-output (func guess-input)))
            (print-out count guess-output)
            (if (close-enough? guess-input guess-output)
                guess-output
                (try guess-output (+ count 1)))))
    (try initial-guess 1))

; trying to find solution to x^x = 1000
; use x => log(1000) / log(x)
(define (solver x) (/ (log 1000) (log x)))
(define (find-solution initial-guess tolerance)
    (fixed-point solver initial-guess tolerance))

; try again, but with average damping
(define (find-solution-average-damping initial-guess tolerance)
    (fixed-point (lambda (x) (average x (solver x))) initial-guess tolerance))

; with this tolerance, average damping took 15 steps while without took 48
(display (find-solution 1.1 0.0000001)) (newline)
(display (find-solution-average-damping 1.1 0.0000001)) (newline)

; with this tolerance, average damping took 10 steps while without took 26
(display (find-solution 1.1 0.001)) (newline)
(display (find-solution-average-damping 1.1 0.001)) (newline)
