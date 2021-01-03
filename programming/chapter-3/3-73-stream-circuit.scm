(load "sect3-5-stream/stream.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C)
  (lambda (currents v0 dt)
     (add-streams
      (integral
        (scale-stream currents (/ 1 C))
        v0
        dt)
       (scale-stream currents R))))

(define RC1 (RC 5 1))

(define currents1
    (append-streams
      (stream-enumerate-interval-dx 0.0 5 0.1)
      (stream-constant-length 5 50)
      (stream-enumerate-interval-dx 5.0 0 -0.1)
      (stream-constant-length 0.0 0)))

; (display-stream currents1)
(display-stream (RC1 currents1 0.0 0.1))
