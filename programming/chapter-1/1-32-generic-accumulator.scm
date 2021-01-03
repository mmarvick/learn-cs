(define (identity x) x)
(define (increment x) (+ x 1))

;; Generic accumulator
;; switch between iterative and recursive process as desired
(define (accumulate a b next calculate combiner null-value)
    (accumulate-iter-process a b next calculate combiner null-value))

(define (accumulate-recursive-process a b next calculate combiner null-value)
    (if (> a b) null-value
        (combiner (calculate a) (accumulate-recursive-process (next a) b next calculate combiner null-value))))

(define (accumulate-iter-process a b next calculate combiner null-value)
    (define (accumulate-iter accum a)
        (if (> a b) accum
            (accumulate-iter (combiner accum (calculate a)) (next a))))
    (accumulate-iter null-value a))

;; Implementations
;; define sum, product, examples of them, and print some examples of those examples
(define (sum-series a b next calculate)
    (accumulate a b next calculate + 0))

(define (product-series a b next calculate)
    (accumulate a b next calculate * 1))

(define (sum-ints max)
    (sum-series 1 max increment identity))

(define (factorial n)
    (product-series 2 n increment identity))

(display "sum ints") (newline)
(display (sum-ints 1)) (newline)
(display (sum-ints 2)) (newline)
(display (sum-ints 3)) (newline)
(display (sum-ints 4)) (newline)
(display (sum-ints 5)) (newline)

(display "factorial") (newline)
(display (factorial 1)) (newline)
(display (factorial 2)) (newline)
(display (factorial 3)) (newline)
(display (factorial 4)) (newline)
(display (factorial 5)) (newline)