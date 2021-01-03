(load "sect3-5-stream/stream.scm")

(define (sq-pairs s t combine)
  (cons-stream
    (combine (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map
          (lambda (x) (combine (stream-car s) x))
          (stream-cdr t))
        (stream-map
          (lambda (x) (combine x (stream-car t)))
          (stream-cdr s)))
      (sq-pairs
        (stream-cdr s)
        (stream-cdr t)
        combine))))
  
(define (cube-triples s t u)
  (sq-pairs
    (sq-pairs s t list)
    u
    (lambda (x y) (append x (list y)))))

(define (ordered-triples s t u)
  (stream-filter
    (lambda (triple) (and (<= (car triple) (cadr triple))
                          (<= (cadr triple) (caddr triple))))
    (cube-triples s t u)))

(define pythagorean-triples
  (stream-filter
    (lambda (triple) (= (+ (square (car triple))
                           (square (cadr triple)))
                        (square (caddr triple))))
    (ordered-triples
      stream-integers
      stream-integers
      stream-integers)))

(display-stream-to
  pythagorean-triples
  50)