(define nil '())
(define (displayln text)
  (display text)(newline))

(define (make-table same-key?)
  (define table-records (list))
  (define (assoc key)
    (define (assoc-iter key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc-iter key (cdr records)))))
    (assoc-iter key table-records))
  (define (lookup key)
    (let ((record (assoc key)))
      (if record
          (cdr record)
          false)))
  (define (insert! key value)
    (let ((record (assoc key)))
      (if record
        (set-cdr! record value)
        (set! table-records
              (cons (cons key
                          value)
                    table-records)))))
  (define (dispatch m)
    (cond ((eq? m 'lookup) lookup)
          ((eq? m 'insert!) insert!)
          (else "Invalid table message - " m)))
  dispatch)

(define (insert! key value table)
  ((table 'insert!) key value))

(define (lookup key table)
  ((table 'lookup) key))

(define t1
  (make-table
    (lambda (x y)
      (< (abs (- x y)) 0.01))))

(insert! 3.14159 'pi t1)
(insert! 1.41421 'root2 t1)
(insert! 2.71828 'e t1)
(displayln (lookup 3.14159 t1))
(displayln (lookup 1.41 t1))
(displayln (lookup 2.71 t1))
(displayln (lookup 2.72 t1))
(displayln (lookup 2.73 t1))
