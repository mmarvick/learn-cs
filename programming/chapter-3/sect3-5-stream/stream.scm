(define (stream-scope s title)
  (stream-map
    (lambda (x)
      (display title)
      (display ": ")
      (display x)
      (newline)
      x)
    s))

(define (stream-constant n)
  (cons-stream n (stream-constant n)))

(define (stream-constant-length n length)
  (if (= length 0) the-empty-stream
                   (cons-stream n (stream-constant-length n (- length 1)))))

(define (stream-integers-from n)
  (cons-stream n (stream-integers-from (+ n 1))))

(define stream-integers (stream-integers-from 1))

(define (stream-enumerate-interval low high)
  (stream-enumerate-interval-dx low high 1))

(define (stream-enumerate-interval-dx start finish dx)
  (if (or (and (> dx 0) (> start finish))
           (and (< dx 0) (< start finish)))
      the-empty-stream
      (cons-stream
        start
        (stream-enumerate-interval-dx (+ start dx) finish dx))))

(define (display-stream-item item)
    (display item)
    (newline))

(define (display-stream s)
  (stream-for-each
    display-stream-item
    s))

(define (display-stream-to s n)
  (if (> n 0)
      (if (stream-null? s)
          (begin
            (display "--stream exhausted--")
            (newline))
          (begin
            (display-stream-item (stream-car s))
            (display-stream-to (stream-cdr s) (- n 1))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (add-streams . streams)
  (apply stream-map (append (list +) streams)))

(define (mul-streams . streams)
  (apply stream-map (append (list *) streams)))

(define (sum-stream-to stream n)
  (if (= n 0)
      0
      (+ (stream-car stream)
         (sum-stream-to (stream-cdr stream) (- n 1)))))

(define (scale-stream stream factor)
  (stream-map
    (lambda (x) (* x factor))
    stream))

(define (append-streams . streams)
  (cond ((null? streams) the-empty-stream)
        ((stream-null? (car streams)) (apply append-streams (cdr streams)))
        (else (cons-stream (stream-car (car streams))
                           (apply append-streams (append (list (stream-cdr (car streams)))
                                                 (cdr streams)))))))

(define (merge-streams . streams)
  (define identity (lambda (x) x))
  (apply merge-streams-by
    (append (list identity) streams)))

(define (merge-streams-by fn . streams)
  (define (stream-not-null? stream)
    (not (stream-null? stream)))
  (define (progress-if-equal stream val)
    (if (eq? (stream-car stream) val)
           (stream-cdr stream)
           stream))
  (define (min-by fn values)
    (define (min-by-iter values min-val min-score)
      (cond ((null? values) min-val)
            ((or (null? min-val)
                (< (fn (car values)) min-score))
                (min-by-iter
                  (cdr values)
                  (car values)
                  (fn (car values))))
            (else
                (min-by-iter
                  (cdr values)
                  min-val
                  min-score))))
    (min-by-iter values '() '()))
  (define (merge-streams streams)
    (let ((non-empty-streams (filter stream-not-null? streams)))
      (if (null? non-empty-streams)
          the-empty-stream
          (let ((next-val (min-by fn (map stream-car non-empty-streams))))
            (cons-stream
              next-val
              (merge-streams
                (map
                  (lambda (stream) (progress-if-equal stream next-val))
                  non-empty-streams)))))))
  (merge-streams streams))

(define (partial-sums stream)
  (define summed-stream
    (cons-stream
      (stream-car stream)
      (add-streams (stream-cdr stream)
                   summed-stream)))
  summed-stream)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2
                               (stream-cdr s1)))))

(define (stream-limit s tolerance)
  (cond ((stream-null? s) s)
        ((stream-null? (stream-cdr s)) s)
        (else
          (let ((s1 (stream-car s))
                (s2 (stream-car (stream-cdr s))))
            (if (< (abs (- s2 s1)) tolerance)
                s2
                (stream-limit (stream-cdr s) tolerance))))))
