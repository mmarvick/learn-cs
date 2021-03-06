(define *op-table* (make-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

(define variable? symbol?)
(define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

(define (install-sum-package)
    (define addend car)
    (define augend cadr)
    (define (make addend augend)
        (cond ((eq? addend 0) augend)
            ((eq? augend 0) addend)
            (else (list '+ addend augend))))
    (define (deriv-sum operands var)
        ((get 'make '+)
            (deriv (addend operands) var)
            (deriv (augend operands) var)))
    (put 'deriv '+ deriv-sum)
    (put 'make '+ make))

(define (install-product-package)
    (define multiplier car)
    (define multiplicand cadr)
    (define (make multiplier multiplicand)
        (cond ((or (eq? multiplier 0) (eq? multiplicand 0)) 0)
            ((eq? multiplier 1) multiplicand)
            ((eq? multiplicand 1) multiplier)
            (else (list '* multiplier multiplicand))))
    (define (deriv-prod operands var)
        ((get 'make '+)
            ((get 'make '*)
                (multiplier operands)
                (deriv (multiplicand operands) var))
            ((get 'make '*)
                (multiplicand operands)
                (deriv (multiplier operands) var))))
    (put 'deriv '* deriv-prod)
    (put 'make '* make))

(define (install-exponentiation-package)
    (define base car)
    (define exponent cadr)
    (define (make base exponent)
        (cond ((eq? exponent 0) 1)
              ((eq? base 0) 0)
              (else (list '** base exponent))))
    (define (deriv-exp operands var)
        ((get 'make '*)
            ((get 'make '*)
                (exponent operands)
                ((get 'make '**)
                    (base operands)
                    (- (exponent operands) 1)))
            (deriv (base operands) var)))
    (put 'deriv '** deriv-exp)
    (put 'make '** make))

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (car exp)) (cdr exp) var))))

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(display (deriv '(+ x 3) 'x))(newline)
(display (deriv (list '+ 'x 3) 'x))(newline)
(display (deriv '(* x y) 'x))(newline)
(display (deriv (list '* (list '* 'x 'y) (list '+ 'x 3)) 'x))(newline)
(display (deriv '(** x 5) 'x))(newline)
