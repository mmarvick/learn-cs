(load "sect3-5-stream/stream.scm")

(define (ln2-term n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-term (+ 1 n)))))

(define ln2-stream
  (partial-sums (ln2-term 1)))

(display-stream-to ln2-stream 10000)
