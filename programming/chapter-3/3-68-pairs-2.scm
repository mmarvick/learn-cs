; I expect this to work.
;   stream-map should paint everything in a row.
;   recursively calling pairs should inset one row and column and paint from there
; we'll end up building up the diagonal faster, so we'll have some "overhang"
;
; I was wrong! We get ourselves into trouble because we make the recursive call to pairs
; now without being in the cdr of a cons-stream, meaning it isn't delayed. So we just go
; recursively down the diagonal and never start building a stream.
(load "sect3-5-stream/stream.scm")

(define (pairs s t)
  (display "in it")(newline)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

(display-stream-to
  (pairs
    (stream-enumerate-interval 1 100)
    (stream-enumerate-interval 1 100))
  20)