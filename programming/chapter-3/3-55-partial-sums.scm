(load "sect3-5-stream/stream.scm")

(define (partial-sums stream)
  (define summed-stream
    (cons-stream
      (stream-car stream)
      (add-streams (stream-cdr stream)
                   summed-stream)))
  summed-stream)

(display-stream-to (partial-sums (integers-starting-from 1)) 10)
