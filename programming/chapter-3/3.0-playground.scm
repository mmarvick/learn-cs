(define x 10)

(define myFn (lambda args x))

(display (myFn))(newline)

(let ((x 5))
  (display x)(newline)
  (display (myFn))(newline))