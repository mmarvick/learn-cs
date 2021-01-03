(define (square n) (* n n))

(define (expmod base exp mod)
    (cond ((= exp 0) 1)
        ((even? exp) (modulo (square (expmod base (/ exp 2) mod)) mod))
        (else (modulo (* base (expmod base (- exp 1) mod)) mod))))

(define (fast-prime? n times)
    (define (fermat-test a)
        (= (expmod a n n) a))
    (define (fermat-test-random)
        (define (get-rand) (+ 1 (random (- n 1))))
        (fermat-test (get-rand)))
    (define (fermat-loop times-left)
        (cond ((= 0 times-left) true)
            ((fermat-test-random) (fermat-loop (- times-left 1)))
            (else false)))
    (fermat-loop times)
)

(define (timed-prime-test n times)
    (define (start-prime-test start-time)
        (if (fast-prime? n times)
            (report-prime start-time (runtime))))
    (define (report-prime start-time end-time)
        (display n)
        (display " *** ")
        (display (- end-time start-time))
        (newline))

    (start-prime-test (runtime))
)

(define (search-for-primes start times)
    (timed-prime-test start times)
    (search-for-primes (+ 1 start) times))

; (search-for-primes 100000000     1)  ; about 0 s
; (search-for-primes 10000000000   1)  ; about 0 s
; (search-for-primes 1000000000000 1)  ; about 0  s

; about 0.11  s
(search-for-primes 10000000000000000000000000000000000000000000000000000000000000 100)
; about 0.26  s
(search-for-primes 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 100)
; about 0.44 s
(search-for-primes 1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 100)  ; about 0.26  s
; about 0.74 s
(search-for-primes 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 100)


; I had to bump the number of iterations checks to 100 just to get registerable time!
; It seems to be growing faster than O(log N).
    ; x^2 seems to take about 2.4x
    ; x^3 seems to take about 4x
    ; x^4 seems to take about 6.7x
    ; and even going from (x^2)^2, I see about 2.8x
