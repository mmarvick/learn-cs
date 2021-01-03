(load-option 'format)

(define (square n) (* n n))
(define (divides? a b) (= 0 (modulo a b)))

(define (smallest-divisor n)
    (smallest-divisor-iter n 2))

(define (smallest-divisor-iter n d)
    ; (show-sd-output n d)
    (cond ((> (square d) n) n)
        ((divides? n d) d)
        (else (smallest-divisor-iter n (+ d 1))))
)

(define (show-sd-output n d)
    (format
        #t
        "In smallest-divisor-iter with params ~S ~S~%"
        n d)
)

(display (smallest-divisor 1)) (newline)
(display (smallest-divisor 2)) (newline)
(display (smallest-divisor 3)) (newline)
(display (smallest-divisor 4)) (newline)
(display (smallest-divisor 5)) (newline)
(display (smallest-divisor 15)) (newline)
(display (smallest-divisor 31)) (newline)
(display (smallest-divisor 199)) (newline)
(display (smallest-divisor 1999)) (newline)
(display (smallest-divisor 19999)) (newline)
