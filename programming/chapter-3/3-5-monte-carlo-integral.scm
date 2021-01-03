(define square (lambda (x) (* x x)))
(define (random-in-range low high)
  (let ((range (* 1.0 (- high low))))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
            (/ trials-passed trials))
          ((experiment)
            (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (define (simulation)
    (let ((percent (monte-carlo trials test))
          (area (* (- x2 x1) (- y2 y1))))
      (* 1.0 percent area)))
  (simulation))
  

; test whether a point is within the circle radius 3 centered at point 5, 7
(define (circle-predicate x y)
  (>= 9 (+ (square (- x 5))
          (square (- y 7)))))

(display (estimate-integral circle-predicate 2 8 4 10 100000))(newline)
(display (estimate-integral circle-predicate 0 10 0 10 100000))(newline)