#lang sicp
(#%require sicp-pict)

(paint (segments->painter
        (list
         (make-segment (make-vect 0 0)
                       (make-vect 0 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 1))
         (make-segment (make-vect 1 1)
                       (make-vect 1 0))
         (make-segment (make-vect 1 0)
                       (make-vect 0 0)))))

(paint (segments->painter
        (list
         (make-segment (make-vect 0 0)
                       (make-vect 1 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 0)))))

(paint (segments->painter
        (let ((p1 (make-vect 0 0.5))
              (p2 (make-vect 0.5 1))
              (p3 (make-vect 1 0.5))
              (p4 (make-vect 0.5 0)))
          (list (make-segment p1 p2)
                (make-segment p2 p3)
                (make-segment p3 p4)
                (make-segment p4 p1)))))
