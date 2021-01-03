(define nil '())
(define x (cons (list 1 2) (list 3 4)))
(define y (list x x))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (count-leaves seq)
    (accumulate +
                0
                (map (lambda (node)
                        (cond ((null? node) 0)
                              ((not (pair? node)) 1)
                              (else (count-leaves node))))
                      seq)))

(display (count-leaves x))(newline)
(display (count-leaves y))(newline)
