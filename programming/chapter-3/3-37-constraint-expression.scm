(load "sect3-3-constraint/constraint.scm")

; expression-style constraints
(define (c+ x y)
  (let ((z (make-connector))) 
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

; Celcius -> Fahrehnheit converter using expressions
(define (c-f-exp-converter x)
  (c+
    (c*
      (c/ (cv 9) (cv 5))
      x)
    (cv 32)))

; Test our converter
(define C (make-connector))
(define F (c-f-exp-converter C))
(probe "Celsius temp   " C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

; F -> C converter
(define (f-c-exp-converter x)
  (c*
    (c/ (cv 5) (cv 9))
    (c-
      x
      (cv 32))))
(define F2 (make-connector))
(define C2 (f-c-exp-converter F2))
(probe "2nd Celsius temp   " C2)
(probe "2nd Fahrenheit temp" F2)

(newline)
(set-value! C2 25 'user)
(forget-value! C2 'user)
(set-value! F2 212 'user)