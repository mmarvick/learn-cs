(define nil '())
(define s (list 1 2 3 4 5))

(define (fold-right op init seq)
    (if (null? seq) init
                    (op (car seq)
                        (fold-right op init (cdr seq)))))

(define (fold-left op init seq)
    (define (iter result rest)
        (if (null? rest) result
                         (iter (op result (car rest))
                               (cdr rest))))
    (iter init seq))

(define (reverse-right seq)
    (fold-right (lambda (x y) (append y (list x)))
                nil
                seq))

(define (reverse-left seq)
    (fold-left (lambda (x y) (append (list y) x))
                nil
                seq))

(display (reverse-right s))(newline)
(display (reverse-left s))(newline)
