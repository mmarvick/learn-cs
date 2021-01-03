(define (make-interval a b) (cons a b))
(define (make-center-width center width) (cons (- center width) (+ center width)))
(define (make-center-percent center percent) (make-center-width center (* center percent)))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (center i) (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i) (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i) (/ (width i) (center i)))

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
    (let ((lower-b (lower-bound b)) (upper-b (upper-bound b)))
        (cond ((or (= 0 lower-b) (= 0 upper-b)) (error "You cannot divide by a range that contains 0"))
              ((> 0 (* upper-b lower-b)) (error "You cannot divide by a range that spans 0"))
              (else (mul-interval a
                    (make-interval (/ 1.0 (upper-bound b))
                                    (/ 1.0 (lower-bound b))))))))

(let ((interval (make-center-width 3 5)))
        (display "interval:  ")(print-interval interval)
        (display "center:    ")(display (center interval))(newline)
        (display "width:     ")(display (width interval))(newline))

(newline)

(let ((interval (make-center-percent 5 .1)))
        (display "interval:  ")(print-interval interval)
        (display "center:    ")(display (center interval))(newline)
        (display "width:     ")(display (width interval))(newline)
        (display "percent:   ")(display (percent interval))(newline))
