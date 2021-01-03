(load "sect3-5-stream/stream.scm")

(define (random-stream requests)
  ; note - this approach isn't deteministic between runs of the program since we're not using stream-update.
  ; we generate a random stream using (random) and don't have a seed
  ; however, this random stream is used by the stream processing random requests, so resetting will still return
  ; to the start of the _generated_ random stream
  (define (make-random-stream)
    (cons-stream (random 1.0)
                 (make-random-stream)))
  (let ((ran-stream (make-random-stream)))
    (define (iter ran-cur req-cur)
      (if (stream-null? req-cur)
          the-empty-stream
          (let ((this-request (stream-car req-cur))
                (remaining-requests (stream-cdr req-cur)))
            (cond ((eq? this-request 'generate)
                      (cons-stream (stream-car ran-cur)
                                  (iter
                                      (stream-cdr ran-cur)
                                      remaining-requests)))
                  ((eq? this-request 'reset)
                      (cons-stream (stream-car ran-stream)
                                  (iter
                                      (stream-cdr ran-stream)
                                      remaining-requests)))
                  (else (error "INVALID REQUEST - " this-request))))))
    (iter ran-stream requests)))

(display-stream
  (random-stream
    (stream 'generate 'generate 'generate
               'reset 'generate 'generate 'generate 'generate
               'reset
               'reset 'generate
               'reset)))

