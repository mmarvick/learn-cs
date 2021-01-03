(define (make-accumulator init)
  (let ((value init))
    (lambda (decrement)
      (set! value (- value decrement))
      value)))

(define (make-accumulator-2 init)
  ((lambda (value)
    (lambda (decrement)
      (set! value (- value decrement))
      value
    )) init))

(let ((A (make-accumulator 25))
      (B (make-accumulator 40)))
  (display (A 10))(newline)
  (display (A 5))(newline)
  (display (B 10))(newline)
  (display (B 10))(newline))