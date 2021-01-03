(define (last-pair list)
    (let ((right (cdr list)))
        (if (null? right) list
                          (last-pair right))))

(display (last-pair (list 1 4 9 16 25)))(newline)
