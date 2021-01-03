(load "sect3-5-stream/stream.scm")

(define square (lambda (x) (* x x)))
(define (random-in-range low high)
  (let ((range (* 1.0 (- high low))))
    (+ low (random range))))

(define (monte-carlo experiment)
  (define (experiment-results-generator)
    (cons-stream (experiment) (experiment-results-generator)))
  (define experiment-results (experiment-results-generator))
  (define count-passed
    (cons-stream 0 (stream-map
                    (lambda (result prev-count)
                      (if result
                          (+ prev-count 1)
                          prev-count))
                    experiment-results
                    count-passed)))
  (define count-experiments (stream-integers-from 0))
  (stream-map
    (lambda (passed total) (/ (* passed 1.0) total))
    (stream-cdr count-passed)
    (stream-cdr count-experiments)))

(define (estimate-integral P x1 x2 y1 y2)
  (define (test)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (scale-stream (monte-carlo test) area)))
  

; test whether a point is within the circle radius 3 centered at point 5, 7
(define (circle-predicate x y)
  (>= 9 (+ (square (- x 5))
          (square (- y 7)))))

(define sim-1 (estimate-integral circle-predicate 2 8 4 10))
(define sim-2 (estimate-integral circle-predicate 0 10 0 10))

(display (stream-ref sim-1 100000))(newline)
(display (stream-ref sim-2 100000))(newline)

(newline)
(display-stream-to
  (estimate-integral circle-predicate 2 8 4 10)
  100)
; (display-stream-to
;   (estimate-integral circle-predicate 0 10 0 10)
;   100)