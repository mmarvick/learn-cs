(define square (lambda (x) (* x x)))

(define (square-list-1 items)
    (if (null? items)
        '()
        (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
    (map square items))

(define items (list 1 2 3 4 5 6 7))
(display (square-list-1 items))(newline)
(display (square-list-2 items))(newline)
