(load "sect3-5-stream/stream.scm")

; the problem in the book is that we're passing forward the rolling average (average of prev and current value)
; as the prev value to be used in the next step.
; we should be passing forward the _raw_ previous value (used to calculate the next average value), as well as the
; previous average value (to use as the previous input to the sign change detector)

(define input (stream 1.5 2 -0.2 1 -0.5 -1 0.1 -0.5 0.5 4))

(define (sign-change-detector y x)
  (cond ((and (<= x 0) (> y 0)) 1)
        ((and (>= x 0) (< y 0)) -1)
        (else 0)))

(define (zero-crossings sense-data)
  (define (make-zero-crossings input-stream last-value last-avg)
    (if (stream-null? input-stream)
        the-empty-stream
        (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
          (cons-stream (sign-change-detector avpt last-avg)
                       (make-zero-crossings (stream-cdr input-stream)
                                            (stream-car input-stream)
                                            avpt)))))
  (make-zero-crossings sense-data 0 (stream-car sense-data)))

(display-stream (zero-crossings input))(newline)