(define nil '())
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
(define v (list 1 2 3 4))
(define w (list 4 3 2 1))

(define (accumulate op init seq)
    (if (null? seq) init
                    (op (car seq)
                        (accumulate op init (cdr seq)))))


(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (m-row) (dot-product m-row v))
         m))

(define (transpose m)
    (if (null? (car m))
            nil
            (cons (map (lambda (l) (car l))
                       m)
                  (transpose (map (lambda (l) (cdr l))
                                  m)))))

(define (matrix-*-matrix m n)
    (map (lambda (m-row)
        (map (lambda (n-col) (dot-product m-row n-col))
             (transpose n)))
    m))

(define (matrix-*-matrix-efficient m n)
    (define (matrix-*-matrix-dot m n)
        (map (lambda (m-row)
            (map (lambda (n-col) (dot-product m-row n-col))
                 n))
             m))
    (matrix-*-matrix-dot m (transpose n)))

(display (dot-product v w))(newline)     ; expect: 20
(display (matrix-*-vector m v))(newline) ; expect: (20 56 80)
(display (transpose m))(newline)         ; expect: ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
(display (matrix-*-matrix m n))(newline) ; expect: ((50 60) (91 112) (130 150))
(display (matrix-*-matrix-efficient m n))(newline) ; expect: ((50 60) (91 112) (130 150))
