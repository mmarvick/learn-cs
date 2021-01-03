(define (sum args)
    (if (null? args) 0
                     (+ (car args) (sum (cdr args)))))

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (scale-vect v s)
  (make-vect
   (* (xcor-vect v) s)
   (* (ycor-vect v) s)))

(define (add-vect . v)
  (make-vect
    (sum (map xcor-vect v))
    (sum (map ycor-vect v))))

(define (frame-coordinate-map frame)
    (lambda (v)
        (add-vect
            (origin-frame frame)
            (scale-vect (edge1-frame frame)
                        (xcor-vect v))
            (scale-vect (edge2-frame frame)
                        (ycor-vect v)))))

(define (segments->painter segment-list)
  (lambda (frame)
    (let ((coord-map (frame-coordinate-map frame)))
      (for-each
       (lambda (segment)
         (draw-line
          (coord-map (start-segment segment))
          (coord-map (end-segment segment))))
       segment-list))))