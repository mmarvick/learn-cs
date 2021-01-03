(define (square n) (* n n))
(define (divides? a b) (= 0 (modulo a b)))

(define (smallest-divisor n)
    (define (next-divisor n)
        (if (< n 3) (+ n 1) (+ n 2)))
    (define (smallest-divisor-iter d)
        (cond ((> (square d) n) n)
            ((divides? n d) d)
            (else (smallest-divisor-iter (next-divisor d)))))

    (smallest-divisor-iter 2)
)

(define (prime? n) (= n (smallest-divisor n)))

(define (timed-prime-test n)
    (define (start-prime-test start-time)
        (if (prime? n)
            (report-prime start-time (runtime))))
    (define (report-prime start-time end-time)
        (display n)
        (display " *** ")
        (display (- end-time start-time))
        (newline))

    (start-prime-test (runtime))
)

(define (search-for-primes start)
    (timed-prime-test start)
    (search-for-primes (+ 1 start)))

(search-for-primes 100000000)     ; about 1.0e-2 s
(search-for-primes 10000000000)   ; about 1.0e-1 s
(search-for-primes 1000000000000) ; about 1.0e0  s

; Incrementing by 2's seems to get us about 40% improvement. It's probably not exactly half, because we added extra branching logic
