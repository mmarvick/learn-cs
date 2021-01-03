(define nil '())

(define (accumulate op init seq)
    (if (null? seq) init
                    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap op seq)
    (accumulate append nil (map op seq)))

(define (enumerate-interval i n)
    (if (= n i) (list n)
                (append (enumerate-interval i (- n 1)) (list n))))

(define (unique-pairs n)
    (flatmap (lambda (i)
            (map (lambda (j) (list i j))
                (enumerate-interval 1 i)))
         (enumerate-interval 1 n)))

(define (prime? n)
    (define (divides? n d) (= 0 (modulo n d)))
    (define (smallest-divisor n)
        (define (smallest-divisor-iter d)
            (cond ((> (square d) n) n)
                ((divides? n d) d)
                (else (smallest-divisor-iter (+ d 1)))))
        (smallest-divisor-iter 2))
    (= n (smallest-divisor n)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (let ((x (car pair)) (y (cadr pair)))
        (list x y (+ x y))))

(define (prime-sum-pairs n)
    (map
        make-pair-sum
        (filter
            prime-sum?
            (unique-pairs 5))))

(display (prime-sum-pairs 5))(newline)
