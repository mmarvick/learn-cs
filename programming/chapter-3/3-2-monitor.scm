(define square (lambda (x) (* x x)))

(define (make-monitored f)
  (let ((count 0))
    (lambda args
      (if (and (not (null? args))
               (eq? (car args) 'how-many-calls?))
          count
          (begin
            (set! count (+ count 1))
            (apply f args))))))

(define s (make-monitored square))

(display (s 10))(newline)
(display (s 12))(newline)
(display (s 'how-many-calls?))(newline)
