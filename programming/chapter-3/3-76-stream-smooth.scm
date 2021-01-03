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

(define (smoothe stream)
  (define (make-smoothe input prev-value)
    (if (stream-null? input)
        the-empty-stream
        (let ((avpt (/ (+ (stream-car input) prev-value) 2)))
          (cons-stream avpt
                      (make-smoothe (stream-cdr input)
                                    (stream-car input))))))
  (make-smoothe input (stream-car input)))

; this is the same algorithm, but looks ahead instead of passing down the previous value
(define (smoothe2 stream)
  (define (make-smoothe input)
    (if (stream-null? (stream-cdr input))
        the-empty-stream
        (let* ((current (stream-car input))
               (next (stream-car (stream-cdr input)))
               (avpt (/ (+ current next) 2)))
          (cons-stream avpt (make-smoothe (stream-cdr input))))))
  (if (stream-null? input)
      the-empty-stream
      (cons-stream (stream-car input)
                   (make-smoothe input))))

(define (zero-crossings sense-data)
  (let ((smoothed (smoothe sense-data)))
    (stream-map
      sign-change-detector
      smoothed
      (cons-stream (stream-car smoothed) smoothed))))

; (display-stream (smoothe input))(newline)
; (display-stream (smoothe2 input))
(display-stream (zero-crossings input))