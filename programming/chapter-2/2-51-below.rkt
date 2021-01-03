#lang sicp
(#%require sicp-pict)

(define (below bottom top)
  (let ((painter-b
         (transform-painter bottom
                            (make-vect 0 0)
                            (make-vect 1 0)
                            (make-vect 0 0.5)))
        (painter-t
         (transform-painter top
                            (make-vect 0 0.5)
                            (make-vect 1 0.5)
                            (make-vect 0 1))))
    (lambda (frame)
      (painter-b frame)
      (painter-t frame))))

(define (below-alt bottom top)
  (rotate270
   (beside
    (rotate90 bottom)
    (rotate90 top))))

(paint (below einstein einstein))
(paint (below-alt einstein einstein))
