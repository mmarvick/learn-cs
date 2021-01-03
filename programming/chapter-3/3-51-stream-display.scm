; I wasn't thinking ahead here until running it!

; just defining will print 0 -- which makes sense because `show` gets called to instantiate
; the stream, since the procedure has to be called on the car

; calling stream-ref 5 will call `show` from 1...5, so they get printed. displaying that value
; prints another 5 (which is the return value from show)

; calling stream-ref 7 will call `show` from 6...7, so they get printed in addition to another 7

; even though we refer to the whole stream from stream-ref, the delay call is memoized so show
; doesn't get called again. That means that the return value (the value in the stream) is memoized
; without the other logic in the show function (e.g. displaying and newlining) happening again

(load "sect3-5-stream/stream.scm")

(define (show x)
  (display x)
  (newline)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))(newline)
(display (stream-ref x 5))(newline)(newline)
(display (stream-ref x 7))(newline)(newline)
(display (stream-ref x 5))(newline)(newline)