(define (equal? x y)
    (cond ((and (not (pair? x)) (not (pair? y)))
               (eq? x y))
          ((and (pair? x) (pair? y))
                (and (equal? (car x) (car y))
                     (equal? (cdr x) (cdr y))))
          (else
                #f)))

(display (equal? 2 2))(newline)
(display (equal? 2 3))(newline)
(display (equal? '(this is a list) '(this is a list)))(newline)
(display (equal? '(this is a list) '(this is the list)))(newline)
(display (equal? '(this is a list) '(this (is a) list)))(newline)