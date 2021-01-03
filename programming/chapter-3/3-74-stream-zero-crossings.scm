(load "sect3-5-stream/stream.scm")

(define input (stream 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define (sign-change-detector y x)
  (cond ((and (< x 0) (> y 0)) 1)
        ((and (> x 0) (< y 0)) -1)
        (else 0)))

(define (zero-crossings sense-data)
  (define (make-zero-crossings input-stream previous-value)
    (if (stream-null? input-stream)
        the-empty-stream
        (cons-stream
          (sign-change-detector (stream-car input-stream) previous-value)
          (make-zero-crossings (stream-cdr input-stream)
                              (stream-car input-stream)))))
  (make-zero-crossings sense-data 0))

(define (zero-crossings-2 sense-data)
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display-stream (zero-crossings input))(newline)
(display-stream (zero-crossings-2 input))