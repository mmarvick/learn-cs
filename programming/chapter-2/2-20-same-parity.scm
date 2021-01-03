(define even? (lambda (x) (= 0 (modulo x 2))))
(define odd?  (lambda (x) (= 1 (modulo x 2))))

(define (filter list predicate?)
    (cond ((null? list) '())
          ((predicate? (car list))
            (cons (car list) (filter (cdr list) predicate?)))
          (else (filter (cdr list) predicate?))))

(define (same-parity x . l)
    (if (even? x) (cons x (filter l even?))
                  (cons x (filter l odd?))))

(display (same-parity 1 2 3 4 5 6 7))(newline)
(display (same-parity 2 3 4 5 6 7))(newline)

