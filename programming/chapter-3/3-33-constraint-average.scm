(load "sect3-3-constraint/constraint.scm")

(define (averager a b c)
  (let ((sum (make-connector))
        (div-constant (make-connector)))
    (adder a b sum)
    (multiplier c div-constant sum)
    (constant 2 div-constant)
    'ok))

(define I1 (make-connector))
(define I2 (make-connector))
(define AVERAGE (make-connector))
(averager I1 I2 AVERAGE)
(probe "I1" I1)
(probe "I2" I2)
(probe "AVERAGE" AVERAGE)

(set-value! I1 12 'user)
(set-value! I2 20 'user)

(newline)
(forget-value! I2 'user)
(set-value! AVERAGE 20 'user)
