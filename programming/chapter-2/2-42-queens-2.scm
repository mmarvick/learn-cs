(define nil '())

(define (reverse l)
    (if (null? l)
        nil
        (append (reverse (cdr l)) (list (car l)))))

(define (accumulate op init seq)
    (if (null? seq) init
                    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap op seq)
    (accumulate append nil (map op seq)))

(define (enumerate-interval i n)
    (if (= n i) (list n)
                (append (enumerate-interval i (- n 1)) (list n))))

(define (queens board-size)
    (define empty-board nil)
    (define (make-queen column row) (list column row))
    (define (queen-col queen) (car queen))
    (define (queen-row queen) (cadr queen))
    (define (adjoin-position new-row k rest-of-queens)
        (append (list (make-queen k new-row)) rest-of-queens))
    (define (safe? k positions)
        (define (safe-pair? new-row new-col pos-row pos-col)
            (cond ((= new-row pos-row)
                        #f)
                  ((= (+ new-row new-col) (+ pos-row pos-col))
                        #f)
                  ((= (+ (- board-size new-row) new-col) (+ (- board-size pos-row) pos-col))
                        #f)
                  (else
                        #t)))
        (define (safe-with? queen rest-of-queens)
            (cond ((null? rest-of-queens)
                        #t)
                  ((safe-pair?
                    (queen-row queen)
                    (queen-col queen)
                    (queen-row (car rest-of-queens))
                    (queen-col (car rest-of-queens)))
                        (safe-with? queen (cdr rest-of-queens)))
                  (else
                        #f)))
        (safe-with? (car positions) (cdr positions)))
    (define (queens-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                                (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 1 board-size)))
                    (queens-cols (- k 1))))))
    (map
        reverse
        (queens-cols board-size)))

(define (timed-queens n)
    (define (call-queens start)
        (let ((result (queens n)))
            (list result (- (runtime) start))))
    (call-queens (runtime)))

(let ((result (timed-queens 8)))
    (map
        (lambda (result) (begin
            (display result)
            (newline)))
        (car result))
    (display (cadr result))(display " seconds")(newline))
