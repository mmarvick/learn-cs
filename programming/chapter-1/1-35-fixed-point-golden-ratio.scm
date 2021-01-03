(define tolerance 0.0000001)
(define (close-enough? a b) (< (abs (- a b)) tolerance))

(define (fixed-point func guess_input)
    (let ((guess_output (func guess_input)))
        (if (close-enough? guess_input guess_output)
            guess_output
            (fixed-point func guess_output))))

(define (golden-ratio initial-guess)
    (* 1.0 (fixed-point (lambda (x) (+ 1 (/ 1 x))) initial-guess)))

(display (golden-ratio 1)) (newline)
