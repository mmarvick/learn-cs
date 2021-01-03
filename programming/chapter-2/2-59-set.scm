(define set '(1 2 3 4))
(define set2 '(4 7 2 6))

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((eq? (car set) x) true)
          (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (intersect-set s1 s2)
    (cond ((null? s1)
            '())
          ((element-of-set? (car s1) s2)
            (cons (car s1) (intersect-set (cdr s1) s2)))
          (else
            (intersect-set (cdr s1) s2))))

(define (union-set s1 s2)
    (cond ((null? s1)
            s2)
          ((element-of-set? (car s1) s2)
            (union-set (cdr s1) s2))
          (else
            (cons (car s1) (union-set (cdr s1) s2)))))

(display (element-of-set? 3 set))(newline)
(display (element-of-set? 5 set))(newline)
(display (adjoin-set 2 set))(newline)
(display (adjoin-set 8 set))(newline)
(display (intersect-set set set2))(newline)
(display (union-set set set2))(newline)
