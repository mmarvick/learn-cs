(load "sect3-5-stream/stream.scm")

(define S (cons-stream 1 (merge-streams
                            (scale-stream S 2)
                            (scale-stream S 3)
                            (scale-stream S 5))))

(display-stream-to S 15)
