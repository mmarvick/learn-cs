(load "sect3-5-stream/stream.scm")

(define (exponentiated-series x)
  (define (iter n)
    (cons-stream (expt x n) (iter (+ n 1))))
  (iter 0))

(define (integrate-series s)
  (define (iter s n)
    (cons-stream
      (/ (stream-car s) n)
      (iter (stream-cdr s) (+ n 1))))
  (iter s 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (mul-streams
                                      sine-series
                                      (stream-constant -1)))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (power-series s x n)
  (sum-stream-to
    (mul-streams
      (exponentiated-series x)
      s)
    n))

(display (* 1.0 (power-series sine-series 1 100)))(newline)
(display (* 1.0 (power-series cosine-series 1 100)))(newline)
