(define (sum args)
    (if (null? args) 0
                     (+ (car args) (sum (cdr args)))))

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (scale-vect v s)
  (make-vect
   (* (xcor-vect v) s)
   (* (ycor-vect v) s)))

(define (add-vect . v)
  (make-vect
    (sum (map xcor-vect v))
    (sum (map ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect
    v1
    (scale-vect v2 -1)))

(define v1 (make-vect 2 3))
(define v2 (make-vect 1 7))

(display v1)(newline)
(display v2)(newline)
(display (add-vect v1 v2))(newline)
(display (add-vect v1 v2 v2))(newline)
(display (sub-vect v1 v2))(newline)
(display (scale-vect v1 -3))(newline)
