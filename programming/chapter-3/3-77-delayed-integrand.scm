; I struggled with this one for a while -- the premise of delaying the integrand and forcing it to evaluate in a let was easy enough,
; but I forgot to use delay in the recursive call until a lot of poking around. I know this isn't supposed to be the takeaway from this
; book, but this is where a typesystem might help :)    Or at least better debug tools.

(load "sect3-5-stream/stream.scm")

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                  (if (stream-null? integrand)
                      the-empty-stream
                      (integral (delay (stream-cdr integrand))
                                (+ (* dt (stream-car integrand))
                                    initial-value)
                                dt)))))

(display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))(newline)