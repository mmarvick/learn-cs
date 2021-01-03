(define (stream-limit s tolerance)
  (cond ((stream-null? s) s)
        ((stream-null? (stream-cdr s)) s)
        (else
          (let ((s1 (stream-car s))
                (s2 (stream-car (stream-cdr s))))
            (if (< (abs (- s2 s1)) tolerance)
                s2
                (stream-limit (stream-cdr s) tolerance))))))

(define (average . args)
  (/ (apply + args)
     (length args)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                    (lambda (guess) (sqrt-improve guess x))
                    guesses)))
  guesses)

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


(display (sqrt 1000 10))(newline)
(display (sqrt 1000 0.1))(newline)
(display (sqrt 1000 0.01))(newline)
(display (sqrt 1000 0.001))(newline)
(display (sqrt 1000 0.000000000000001))(newline)