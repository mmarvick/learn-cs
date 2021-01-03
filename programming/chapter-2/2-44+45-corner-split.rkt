#lang sicp
(#%require sicp-pict)

(define (dir-split stack split)
  (define (impl painter n)
    (if (= n 0)
        painter
        (let ((tiny (impl painter (- n 1))))
          (stack painter (split tiny tiny)))))
  impl)

(define up-split (dir-split below beside))
(define right-split (dir-split beside below))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((rs (right-split painter (- n 1)))
            (us (up-split painter (- n 1)))
            (cs (corner-split painter (- n 1))))
        (beside
         (below
          painter
          (beside us us))
         (below
          (below rs rs)
          cs)))))

(define (square-limit painter n)
  (let ((cs (corner-split painter n)))
    (let ((upper-half (beside (flip-horiz cs) cs)))
      (below (flip-vert upper-half) upper-half))))

(paint (square-limit einstein 4))
