; this question asks you to divise a function so that
; (+ (f 0) (f 1)) returns
;    0 if evaluated left to right
;    1 if evaluated right to left
; I don't have a good way to make things evaluate right to left, so I'll prove this out by changing arg order

(define f
  (let ((value 0))
    (lambda (new-value)
      (let ((old-value value))
        (set! value new-value)
        old-value))))

; It seems that in _actuality_, we evaluate right to left! Which I didn't expect
; Run only one of these:

(display (+ (f 0) (f 1)))(newline)  ; expect 1
; (display (+ (f 1) (f 0)))(newline)  ; expect 0