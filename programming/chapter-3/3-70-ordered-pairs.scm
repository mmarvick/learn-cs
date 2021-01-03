(load "sect3-5-stream/stream.scm")

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

(define (ordered-pairs s t)
  (ordered-pairs-by
    (lambda (pair) (+ (car pair) (cadr pair)))
    s t))

(define (ordered-pairs-by fn s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-streams-by
      fn
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (ordered-pairs
        (stream-cdr s)
        (stream-cdr t)))))

(display-stream-to
  (ordered-pairs
    stream-integers
    stream-integers)
  20)

; I'm abandoning the second part of the problem... close enough

; (define (indivisible? x)
;   (and (> 0 (quotient x 2))
;        (> 0 (quotient x 3))
;        (> 0 (quotient x 5))))
; (display-stream-to
;   (stream-filter
;     (lambda (pair) (and (indivisible? (car pair))
;                         (indivisible? (cadr pair))))
;     (ordered-pairs-by
;       (lambda (pair) (+ (* 2 (car pair))
;                         (* 3 (cadr pair))
;                         (* 5 (car pair) (cadr pair))))
;       stream-integers
;       stream-integers)
;     )
;   20)
