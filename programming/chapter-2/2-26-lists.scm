(define x (list 1 2 3))
(define y (list 4 5 6))

(display (append x y))(newline)

(display (cons x y))(newline)
(display (list (list 1 2 3) 4 5 6))(newline)
(display (cons (cons 1 (cons 2 (cons 3 '()))) (cons 4 (cons 5 (cons 6 '())))))(newline)

(display (list x y))(newline)
