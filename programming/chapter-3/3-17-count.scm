(define nil '())

; normal lists or nested pairs
(define l1 (cons (cons 'a 'b) (cons 'c 'd)))
(define l2 (list 'a 'b 'c))

; first pair points to second pair from both car and cdr
(define l3 (list 'a 'b 'c))
(set-car! l3 (cdr l3))

; infinite loop -- don't try to print this!
(define l4 (list 'a 'b 'c))
(set-cdr! (cddr l4) l4)

(define safe-lists (list l1 l2 l3))
(define lists (list l1 l2 l3 l4))

; bad count pairs
(define (count-pairs-bad x)
  (if (not (pair? x))
    0
    (+ (count-pairs-bad (car x))
       (count-pairs-bad (cdr x))
       1)))

(define (count-pairs x)
  (define explored-pairs nil)
  (define (explored? pair)
    (define (contains? x list)
      (cond ((null? list) #f)
            ((eq? (car list) x) #t)
            (else (contains? x (cdr list)))))
    (contains? pair explored-pairs))
  (define (count-pair-iter x)
    (cond ((not (pair? x)) 0)
          ((explored? x) 0)
          (else
            (set! explored-pairs (cons x explored-pairs))
            (+ (count-pair-iter (car x))
               (count-pair-iter (cdr x))
               1))))
  (count-pair-iter x))

(define (display-map fn list)
  (for-each
    (lambda (x) (display x) (newline))
    (map fn list)))

(display-map count-pairs-bad safe-lists)(newline)
(display-map count-pairs lists)
