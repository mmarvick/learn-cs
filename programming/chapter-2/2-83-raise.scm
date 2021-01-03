(define *op-table* (make-hash-table))
(define (put op type proc) (hash-table/put! *op-table* (list op type) proc))
(define (get op type) (hash-table/get *op-table* (list op type) #f))

(define (attach-tag tag data) (cons tag data))
(define (type-tag number) (car number))
(define (contents number) (cdr number))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (define (raise x) (make-rational x 1))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'raise 'integer raise)
  'done)

(define (install-rational-package)
  (define (gcd x y)
    (if (= y 0)
         x
         (gcd y (modulo x y))))
  (define (make-rat num den)
    (let ((g (gcd num den)))
        (cons
            (/ num g)
            (/ den g))))
  (define numer car)
  (define denom cdr)
  (define (raise x) (make-real-number (/ (* 1.0 (numer x)) (denom x))))
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational raise)
  'done)

(define (install-real-number-package)
  (define (raise x) (make-complex-from-real-imag x 0))
  (define (tag x) (attach-tag 'real-number x))
  (put 'make 'real-number
       (lambda (x) (tag x)))
  (put 'raise 'real-number raise)
  'done)

(define (install-rectangular-package)
  (define make-from-real-imag cons)
  (define (tag x) (attach-tag 'rectangular x))
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-complex-package)
  (install-rectangular-package)
  (define make-from-real-imag
    (get 'make-from-real-imag 'rectangular))
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(install-integer-package)
(install-rational-package)
(install-real-number-package)
(install-complex-package)
(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-real-number n)
  ((get 'make 'real-number) n))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

; could also use apply-generic here
(define (raise n)
  ((get 'raise (type-tag n)) (contents n)))

(let* ((integer1 (make-integer 2))
       (rational1 (raise integer1))
       (real1 (raise rational1))
       (complex1 (raise real1)))
  (display integer1)(newline)
  (display rational1)(newline)
  (display real1)(newline)
  (display complex1)(newline))