(define (length l)
    (if (null? l) 0
        (+ 1 (length (cdr l)))))

(define (count-leaves l)
    (cond ((null? l) 0)
          ((not (pair? l)) 1)
          (else (+ (count-leaves (car l))
                   (count-leaves (cdr l))))))

(define (print-details l)
    (display l)(newline)
    (display (length l))(newline)
    (display (count-leaves l))(newline)(newline))

(define x (cons (list 1 2) (list 3 4)))
(print-details x)
(print-details (list x x))
