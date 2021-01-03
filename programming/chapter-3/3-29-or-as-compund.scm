; the delay of the compound or gate is:
;     2 * invert_delay + and_delay

(define (or-gate o1 o2 output)
  (let ((oi1 (make-wire))
        (oi2 (make-wire))
        (a (make-wire)))
    (inverter o1 oi1)
    (inverter o2 oi2)
    (and-gate oi1 oi2 a)
    (inverter a output)
    'ok))
