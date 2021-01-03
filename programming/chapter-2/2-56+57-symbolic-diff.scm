(define nil '())
(define ops '(+ * **))

(define (itemEqual value) (lamba (x) (eq? x value)))
(define (itemNotEqual value) (lambda (x) (not (eq? x value))))

(define (accumulate op init seq)
    (if (null? seq) init
                    (op (car seq) (accumulate op init (cdr seq)))))
(define (flatmap op seq)
    (accumulate append nil (map op seq)))

(define (makeOpList op . operands) (append (list op) operands))
(define (isOpEq? exp op) (and (pair? exp) (eq? op (car exp))))
(define (isOp? val)
    (not (null? (memq val ops))))

(define (flattenCommutative op operands)
    (flatmap
        (lambda (operand)
            (cond ((not (pair? operand))
                    (list operand))
                  ((not (eq? op (car operand)))
                    (list operand))
                  (else
                    (flattenCommutative op (cdr operand)))))
        operands))

(define variable? symbol?)
(define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

(define (make-sum . operands)
    (let* ((flattened-operands (flattenCommutative '+ operands))
           (valid-operands (filter (itemNotEqual 0) flattened-operands)))
        (cond ((null? valid-operands) 0)
              ((null? (cdr valid-operands)) (car valid-operands))
              (else (append '(+) valid-operands)))))
(define (sum? exp) (isOpEq? exp '+))
(define addend cadr)
(define (augend exp)
    (if (null? (cdddr exp)) (caddr exp)
                            (apply make-sum (cddr exp))))

(define (make-product v1 v2)
    (cond ((or (eq? v1 0) (eq? v2 0))
            0)
          ((eq? v1 1)
            v2)
          ((eq? v2 1)
            v1)
          (else
            (makeOpList '* v1 v2))))
(define (product? exp) (isOpEq? exp '*))
(define multiplier cadr)
(define multiplicand caddr)

(define (make-exponentiation v1 v2)
    (cond ((eq? v2 0) 1)
          ((eq? v1 0) v1)
          (else (makeOpList '** v1 v2))))
(define (exponentiation? exp) (isOpEq? exp '**))
(define base cadr)
(define exponent caddr)

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
          ((exponentiation? exp)
                (make-product
                    (make-product
                        (exponent exp)
                        (make-exponentiation
                            (base exp)
                            (- (exponent exp) 1)))
                    (deriv (base exp) var)))
          (else
                (error "CANNOT DERIVE " exp))))

(display (deriv '(+ x 3) 'x))(newline)
(display (deriv (list '+ 'x 3) 'x))(newline)

(display (deriv '(* x y) 'x))(newline)

(display (deriv (list '* (list '* 'x 'y) (list '+ 'x 3)) 'x))(newline)

(display (deriv '(** x 5) 'x))(newline)
(display (deriv '(+ (* x 5) 3 (** x 2) (* x 2)) 'x))(newline)

(display (make-sum 3 0 'x '(+ 2 (+ x 1) (* 3 1))))(newline)
