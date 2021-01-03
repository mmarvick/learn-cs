(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
    (define (fringe-list l)
        (cond ((null? l) '())
            ((pair? l) (append (fringe-list (car l)) (fringe-list (cdr l))))
            (else (list l))))
    (if (or (pair? x) (null? x)) (fringe-list x) (error "Not a list")))

(display (fringe x))(newline)
(display (fringe (list x x)))(newline)
(display (fringe '()))(newline)
; (display (fringe 3))(newline) ; this should fail