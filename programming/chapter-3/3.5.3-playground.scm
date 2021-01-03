(load "sect3-5-stream/stream.scm")

(define (partial-sums stream)
  (define summed-stream
    (cons-stream
      (stream-car stream)
      (add-streams (stream-cdr stream)
                   summed-stream)))
  summed-stream)

(define (pi-summands n)
  (display "pi-summand number: ")(display n)(newline)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s- (stream-ref s 0))
        (sn (stream-ref s 1))
        (s+ (stream-ref s 2)))
    (cons-stream (- s+ (/ (square (- s+ sn))
                          (+ s- (* -2 sn) s+)))
                 (euler-transform (stream-cdr s)))))

; (display-stream-to pi-stream 10)(newline)
; (display-stream-to (euler-transform pi-stream) 10)


(define (average . args)
  (/ (apply + args)
     (length args)))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                    (lambda (guess) (sqrt-improve guess x))
                    guesses)))
  guesses)

(define (sqrt-stream-slow x)
    (cons-stream 1.0
                 (stream-map
                    (lambda (guess) (sqrt-improve guess x))
                    (sqrt-stream-slow x))))

; (display-stream-to (sqrt-stream 10) 1000)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2
                               (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t))
      (pairs
        (stream-cdr s)
        (stream-cdr t)))))

; (display-stream-to
;   (pairs
;     (stream-enumerate-interval 1 100)
;     (stream-enumerate-interval 1 100))
;   21)


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (integrate fn initial-value dt)
  (integral
    (stream-map
      fn
      (stream-map
        (lambda (x) (* x dt))
        stream-integers))
    initial-value
    dt))

(display-stream-to
  (integrate
    (lambda (x) (* x x))
    0
    .001)
  10000)

