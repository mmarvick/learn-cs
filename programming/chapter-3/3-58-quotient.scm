(load "sect3-5-stream/stream.scm")

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; I expect
; (1 4 2 8 5)
(display-stream-to (expand 1 7 10) 10)

; I expect
; (3 7 5 0 0)
(display-stream-to (expand 3 8 10) 10)