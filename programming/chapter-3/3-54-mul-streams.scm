(load "sect3-5-stream/stream.scm")

(define (mul-streams . streams)
  (apply stream-map (append (list *) streams)))

(define factorials (cons-stream 1
                                (mul-streams factorials (integers-starting-from 2))))

(display-stream-to factorials 12)