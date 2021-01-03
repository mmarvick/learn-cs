(define set1 '(2 3 2 1 3 2 2)) ; represents set (1 2 3)
(define set2 '(1 3 4 3 5 4 3)) ; represents set (1 3 4 5)

; still O(n), but with a larger n (same algo)
(define (element-of-set? x set)
    (cond ((null? set) false)
          ((eq? (car set) x) true)
          (else (element-of-set? x (cdr set)))))

; super fast
(define (adjoin-set x set)
    (cons x set))

; still O(n^2), but with a larger n (same algo)
(define (intersect-set s1 s2)
    (cond ((null? s1) '())
          ((element-of-set? (car s1) s2)
            (cons (car s1) (intersect-set (cdr s1) s2)))
          (else
            (intersect-set (cdr s1) s2))))

; faster - O(n) instead of O(n^2), because append is still O(n). Remember the n is larger though because of dupes
(define (union-set s1 s2)
    (append s1 s2))

(display (element-of-set? 3 set1))(newline)
(display (element-of-set? 5 set1))(newline)
(display (adjoin-set 2 set1))(newline)
(display (adjoin-set 8 set1))(newline)
(display (intersect-set set1 set2))(newline)
(display (union-set set1 set2))(newline)
