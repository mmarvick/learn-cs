(define nil '())

(define (isOpEq? exp op) (and (pair? exp) (eq? op (cadr exp))))

(define variable? symbol?)
(define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

(define (make-sum v1 v2)
    (cond ((eq? v1 0) v2)
          ((eq? v2 0) v1)
          (else (list v1 '+ v2))))
(define (sum? exp) (isOpEq? exp '+))
(define addend car)
(define augend caddr)

(define (make-product v1 v2)
    (cond ((or (eq? v1 0) (eq? v2 0))
            0)
          ((eq? v1 1)
            v2)
          ((eq? v2 1)
            v1)
          (else
            (list v1 '* v2))))
(define (product? exp) (isOpEq? exp '*))
(define multiplier car)
(define multiplicand caddr)

(define (deriv exp var)
    (cond ((number? exp)
                0)
          ((variable? exp)
                (if (same-variable? exp var) 1 0))
          ((sum? exp)
                (make-sum
                    (deriv (addend exp) var)
                    (deriv (augend exp) var)))
          ((product? exp)
                (make-sum
                    (make-product
                        (multiplier exp)
                        (deriv (multiplicand exp) var))
                    (make-product
                        (multiplicand exp)
                        (deriv (multiplier exp) var))))
          (else
                (error "CANNOT DERIVE " exp))))

(display (deriv '(x + 3) 'x))(newline)
(display (deriv '(x * y) 'x))(newline)
(display (deriv '((x * y) * (x + 3)) 'x))(newline)
