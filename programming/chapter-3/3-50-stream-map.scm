(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (define (display-item item)
    (display item)
    (newline))
  (stream-for-each
    display-item
    s))

(define (stream-map proc . argstreams)
  (if (empty-stream? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define stream1 (stream-enumerate-interval  1 10))
(define stream2 (stream-enumerate-interval 11 20))
(define stream3 (stream-enumerate-interval 21 30))

(display-stream (stream-map + stream1 stream2 stream3))
