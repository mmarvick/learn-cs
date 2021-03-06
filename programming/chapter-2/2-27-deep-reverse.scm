(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list (list 3 4) (list 5 6))))

(define (reverse l)
    (if (null? l) '()
                 (append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
    (cond ((null? l)
            '())
          ((pair? (car l))
            (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
          (else
            (append (deep-reverse (cdr l)) (list (car l))))))

(define (deep-reverse-2 l)
    (if (null? l)
            '()
            (let ((reversed-value (if (pair? (car l))
                                    (deep-reverse-2 (car l))
                                    (car l))))
                (append (deep-reverse-2 (cdr l)) (list reversed-value)))))

(display x)(newline)
(display (reverse x))(newline)
(display (deep-reverse x))(newline)
(display (deep-reverse-2 x))(newline)(newline)
(display y)(newline)
(display (reverse y))(newline)
(display (deep-reverse y))(newline)
(display (deep-reverse-2 y))(newline)
