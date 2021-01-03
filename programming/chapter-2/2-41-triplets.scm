(define nil '())

(define (accumulate op init seq)
    (if (null? seq) init
                    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap op seq)
    (accumulate append nil (map op seq)))

(define (enumerate-interval i n)
    (if (= n i) (list n)
                (append (enumerate-interval i (- n 1)) (list n))))

(define (unique-triplets n)
    (flatmap (lambda (i)
        (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 j)))
                 (enumerate-interval 1 i)))
              (enumerate-interval 1 n)))

(define (sums-to? pair n)
    (= n (+ (car pair) (cadr pair) (caddr pair))))

(define (triplets-summing-to s n)
    (filter
        (lambda (pair) (sums-to? pair s))
        (unique-triplets (min n (- s 2)))))

(display (triplets-summing-to 20 10))(newline)
