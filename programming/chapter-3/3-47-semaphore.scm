; pretty hard to manage a list of mutexes... I decided not to continue
; looking at answer, the best way was with one mutex but then behind the acquire, decrement a count (or wait to retry)
(define (semaphore-mutex count)
  (define (make-mutex-list count)
    (if (= count 0)
        nil
        (cons (make-mutex)
              (make-semaphore (- count 1)))))
  (let ((mutex-list (make-mutex-list count))
        (serializer (make-serializer)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
        #| TODO |#)
      (cond ((eq? m 'release)

; much easier with test-and-set style
(define (make-semaphore count)
  (let ((cell (list current-count)))
    (define (the-semaphore s)
      (cond ((eq? s 'acquire)
             (if (test-and-decrement! cell)
                 (the-semaphore 'acquire)))
            ((eq? s 'release)
                 (safe-increment! cell count))))
    the-semaphore))
  
(define (safe-increment! cell max-value)
  (set-car! cell (min
                  (+ 1 (car cell))
                  max-value)))

(define (test-and-decrement! cell)
  (if (> 0 (car cell))
      (begin
        (set-car! cell (- (car cell) 1))
        false)
      true))
