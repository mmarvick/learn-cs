(define (for-each action list)
    (cond ((null? list) '())
          (else (action (car list)) (for-each action (cdr list)))))

(for-each (lambda (x) (display x)(newline))
          (list 57 321 88))