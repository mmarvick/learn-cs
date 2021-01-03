(define (gcd a b)
  (if (= 0 b)
    a
    (gcd b (modulo a b)))
)

(display (gcd 4 2)) (newline)
