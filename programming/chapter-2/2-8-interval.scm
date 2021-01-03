(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (print-interval i)
    (display "(")
    (display (lower-bound i))
    (display ", ")
    (display (upper-bound i))
    (display ")")
    (newline))

(define (add-interval a b)
    (make-interval (+ (lower-bound a) (lower-bound b))
                   (+ (upper-bound a) (upper-bound b))))

(define (sub-interval a b)
    (add-interval a
                  (make-interval (- (upper-bound b))
                                 (- (lower-bound b)))))

(define (mul-interval a b)
    (let ((p1 (* (lower-bound a) (lower-bound b)))
          (p2 (* (lower-bound a) (upper-bound b)))
          (p3 (* (upper-bound a) (lower-bound b)))
          (p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval a b)
    (mul-interval a
                  (make-interval (/ 1.0 (upper-bound b))
                                 (/ 1.0 (lower-bound b)))))

(let ((interval1 (make-interval 2 3))
      (interval2 (make-interval 1 5)))
        (display "p1:  ") (print-interval interval1)
        (display "p2:  ")(print-interval interval2)
        (display "add: ")(print-interval (add-interval interval1 interval2))
        (display "sub: ")(print-interval (sub-interval interval1 interval2))
        (display "mul: ")(print-interval (mul-interval interval1 interval2))
        (display "div: ")(print-interval (div-interval interval1 interval2)))