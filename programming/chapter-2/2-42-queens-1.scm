(define nil '())

(define (accumulate op init seq)
    (if (null? seq) init
                    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap op seq)
    (accumulate append nil (map op seq)))

(define (enumerate-interval i n)
    (if (= n i) (list n)
                (append (enumerate-interval i (- n 1)) (list n))))

(define (queens board-size)
    (define (safe? positions new-row new-col)
        (define (safe-with? pos-row pos-col)
            (cond ((= new-row pos-row)
                        #f)
                  ((= (+ new-row new-col) (+ pos-row pos-col))
                        #f)
                  ((= (+ (- board-size new-row) new-col) (+ (- board-size pos-row) pos-col))
                        #f)
                  (else
                        #t)))
        (define (safe-position? remaining-positions positions-col)
            (cond ((null? remaining-positions)
                        #t)
                  ((not (safe-with? (car remaining-positions) positions-col))
                        #f)
                  (else
                        (safe-position? (cdr remaining-positions) (+ positions-col 1)))))
        (safe-position? positions 1))
    (define (place-queen col positions)
        (if (> col board-size)
            (list positions)
            (flatmap
                (lambda (row)
                    (if (safe? positions row col)
                            (place-queen (+ 1 col) (append positions (list row)))
                            nil))
                (enumerate-interval 1 board-size))))
    (place-queen 1 nil))

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

