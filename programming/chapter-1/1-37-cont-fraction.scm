(define one (lambda (x) 1))
(define identity (lambda (x) x))

(define (cont-frac n d k)
    (cont-frac-iterative n d k))
    ; (cont-frac-recursive n d k))

(define (cont-frac-recursive n d k)
    (define (cont-frac-recursive-step k-cur k-max)
        (let ((n (n k-cur)) (d (d k-cur)))
            (if (= k-cur k-max) (/ n d) (/ n (+ d (cont-frac-recursive-step (+ k-cur 1) k-max))))))
    (cont-frac-recursive-step 1 k))

(define (cont-frac-iterative n d k)
    (define (cont-frac-iterative-step k-cur accum)
        (let ((n (n k-cur)) (d (d k-cur)))
            (let ((div (/ n (+ d accum))))
                (if (= k-cur 1) div (cont-frac-iterative-step (- k-cur 1) div)))))
    (cont-frac-iterative-step k 0))

(define (golden-ratio k)
    (/ 1.0 (cont-frac one one k)))

; actual value: 1.61803

(display "golden-ratio") (newline)
(display (golden-ratio 1)) (newline)
(display (golden-ratio 2)) (newline)
(display (golden-ratio 3)) (newline)
(display (golden-ratio 4)) (newline)
(display (golden-ratio 5)) (newline)
(display (golden-ratio 10)) (newline)
(display (golden-ratio 11)) (newline) ; this gets us within 0.0001

(display (golden-ratio 90000)) (newline) ; this hits maximum recursive depth with recursive solution; not with iterative
    