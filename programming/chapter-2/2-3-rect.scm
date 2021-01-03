(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (rect-area rect) (* (rect-width rect) (rect-height rect)))
(define (rect-perim rect) (+ (rect-width rect) (rect-height rect)))

; rectangles defined by bottom-left and upper-right points
(define (make-rect bottom-left upper-right) (cons bottom-left upper-right))
(define (rect-width rect) (- (x-point (cdr rect)) (x-point (car rect))))
(define (rect-height rect) (- (y-point (cdr rect)) (y-point (car rect))))
(let ((rect (make-rect (make-point 1 1) (make-point 5 7))))
    (display (rect-area rect)) (newline)
    (display (rect-perim rect)) (newline))

; rectangles defined by origin, width, and height
(define (make-rect origin width height) (cons origin (cons width height)))
(define (rect-width rect) (car (cdr rect)))
(define (rect-height rect) (cdr (cdr rect)))
(let ((rect (make-rect (make-point 1 1) 4 6)))
    (display (rect-area rect)) (newline)
    (display (rect-perim rect)) (newline))
