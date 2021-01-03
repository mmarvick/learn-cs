; this will give us powers of 2
; the next item in the stream is the previous item added to itself, and adding something to itself
; is equivalent to doubling. hence, each item is the double of the previous

(load "sect3-5-stream/stream.scm")
(define s (cons-stream 1 (add-streams s s)))

(display-stream-to s 10)