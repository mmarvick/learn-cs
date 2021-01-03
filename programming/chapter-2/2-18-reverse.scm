(define (append list1 list2)
    (if (null? list1)
            list2
            (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
    (if (null? l) '()
                  (append (reverse (cdr l)) (list (car l)))))

(display (reverse (list 1 4 9 16 25)))(newline)
(display (cons 25 (cons 16 (cons 9 (cons 4 (cons 1 '()))))))(newline)
