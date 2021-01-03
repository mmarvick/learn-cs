(define nil '())
(define (square x) (* x x))
(define seq1 (list 0 1 2 3 4))
(define seq2 (list 5 6 7 8 9))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map op sequence)
    (accumulate (lambda (x y) (cons (op x) y))
                nil
                sequence))

(define (append seq1 seq2)
    (accumulate cons
                seq2
                seq1))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y))
                0
                sequence))


(display (map square seq1))(newline)
(display (append seq1 seq2))(newline)
(display (length seq1))(newline)
