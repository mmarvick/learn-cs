; we should use proper constructors / selectors for subtables
; otherwise, can't differentiate a subtable from a record with a list
; also, can't easily print a whole subtable _and_ also a record
;   => if you car the record (to get it out of a list), then you don't have the whole table anymore
;   => if you don't car the record, you can get the whole table printed, but your returned item is in a list

(define nil '())
(define (displayln text)
  (display text)(newline))

(define (make-table)
  (define table (cons '*table (list)))
  (define (assoc key subtable)
    (define (assoc-iter key records)
      (cond ((null? records) false)
            ((eq? key (caar records)) (car records))
            (else (assoc-iter key (cdr records)))))
    (assoc-iter key (cdr subtable)))
  (define (lookup keys)
    (define (lookup-iter keys subtable)
      (if (null? keys)
          (cdr subtable) ;; NOTE - error handling here so they don't return whole tables would be good
          (let ((next-record (assoc (car keys) subtable)))
            (if next-record
                (lookup-iter (cdr keys) next-record)
                false))))
    (lookup-iter keys table))
  (define (insert! keys value)
    (define (create-subtable keys value)
      (if (null? keys)
          value
          (list (car keys) (create-subtable (cdr keys) value))))
    (define (insert-iter! keys value subtable)
      (if (null? keys)
          (set-cdr! subtable value)
          (let ((next-record (assoc (car keys) subtable)))
            (if next-record
              (insert-iter! (cdr keys) value next-record)
              (set-cdr! subtable
                    (cons (create-subtable keys value)
                          (cdr subtable)))))))
    (insert-iter! keys value table))
  (define (dispatch m)
    (cond ((eq? m 'lookup) lookup)
          ((eq? m 'insert!) insert!)
          (else "Invalid table message - " m)))
  dispatch)

(define (insert! key value table)
  ((table 'insert!) key value))

(define (lookup key table)
  ((table 'lookup) key))

(define t1 (make-table))
(insert! '(people me) 'michael t1)
(insert! '(people wife) 'kaity t1)
(insert! '(pets) '(rudy cosmo) t1)
(insert! '(things electronics computer) 'macbook t1)
(insert! '(things kitchen plates) 'need-replacement t1)

(newline)
(displayln (lookup '() t1))
(displayln (lookup '(people) t1))
(displayln (lookup '(people me) t1))
(displayln (lookup '(people wife) t1))
(displayln (lookup '(people brother) t1))

