(define nil '())
(define x (cons (list 1 2) (list 3 4)))
(define y (list x x))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

(define (count-leaves seq)
    (accumulate (lambda (x y) (+ 1 y))
    0
    (enumerate-tree seq)))

(display (count-leaves x))(newline)
(display (count-leaves y))(newline)
