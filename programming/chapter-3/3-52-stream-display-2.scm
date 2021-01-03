(load "sect3-5-stream/stream.scm")

; a question asks whether the behavior would be the same regardless of memoization. At first I
; thought yes, but came to realize this was wrong after reading some online discussion. Without
; memoization, the stream would re-calculate the accum values, and sum would not be starting at 0.
; this is a danger of relying on memoization or not, while using something stateful in a stream. 
; realistically, we shouldn't use stateful calculations in our stream


(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display sum)(newline)
; sum is 1, since we've delayed execution on the rest of the stream

(define y (stream-filter even? seq))
(display sum)(newline)
; sum is 6, since we've processed the stream up until we got to a positive value (1+2+3)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                          seq))
(display sum)(newline)
; sum is 10, since we've processed the stream up untl we've got something divisible by 5 (1+2+3+4)

(stream-ref y 6)(newline)
(display sum)(newline)
; sum is 136, since we've gone through 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136

(newline)
(display-stream z)
(display sum)(newline)
; sum is 210, since we made it to the end
; ...136, 153, 171, 190, 210