(define (square a) (* a a))
(define (sum-sq a b) (+ (square a) (square b)))
(define (sum-smallest-sq a b c)
  (cond ((> a b c) (sum-sq b c))
        ((> b a c) (sum-sq a c))
        (else (sum-sq a b))))
