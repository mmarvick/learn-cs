(load "sect3-5-stream/stream.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                    (add-streams (scale-stream integrand dt)
                                 int))))
  int)

(define (RCL R C L)
  (lambda (vc0 il0 dt)
    (define vc
      (integral
        (delay (scale-stream il (/ -1 C)))
        vc0
        dt))
    (define il
      (integral
        (delay
          (add-streams
            (scale-stream il (/ (- R) L))
            (scale-stream vc (/ 1 L))))
        il0
        dt))
    (cons vc il)))

(define RCL1 (RCL 1 0.2 1))
(define RCL1-eval1 (RCL1 10 0 0.1))

(display-stream-to (cdr RCL1-eval1) 100)
