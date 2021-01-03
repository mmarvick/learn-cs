; this is a doubly linked list
; we have a regular list structure, that the front-ptr points to the head of and the rear-ptr points to the rear of
; the items _in_ the list are a pair, with the car holding the value and the cdr pointing to the previous list node

(define nil '())

(define (make-deque) (cons nil nil))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque x) (set-car! deque x))
(define (set-rear-ptr! deque x) (set-cdr! deque x))

(define (make-item x prev) (cons x prev))
(define (item-value item) (car item))
(define (item-prev item) (cdr item))
(define (set-item-prev! item prev) (set-cdr! item prev))

; deque operations
(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      "EMPTY"
      (item-value (car (front-ptr deque)))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      "EMPTY"
      (item-value (car (rear-ptr deque)))))

(define (front-insert-deque! deque value)
  (let* ((new-item (make-item value nil))
         (new-node (list new-item)))
    (if (empty-deque? deque)
        (begin
          (set-front-ptr! deque new-node)
          (set-rear-ptr! deque new-node)
          deque)
        (begin
          (set-item-prev! (car (front-ptr deque)) new-node)
          (set-cdr! new-node (front-ptr deque))
          (set-front-ptr! deque new-node)
          deque))))

(define (rear-insert-deque! deque value)
  (if (empty-deque? deque)
      (let* ((new-item (make-item value nil))
             (new-node (list new-item)))
        (set-front-ptr! deque new-node)
        (set-rear-ptr! deque new-node)
        deque)
      (let* ((new-item (make-item value (rear-ptr deque)))
             (new-node (list new-item)))
        (set-cdr! (rear-ptr deque) new-node)
        (set-rear-ptr! deque new-node)
        deque)))

(define (front-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Cannot delete from empty deque")
      (let ((next-node (cdr (front-ptr deque))))
        (if (null? next-node)
            (begin
              (set-rear-ptr! deque nil)
              (set-front-ptr! deque nil)
              deque)
            (begin
              (set-item-prev! (car next-node) nil)
              (set-front-ptr! deque (cdr (front-ptr deque)))
              deque)))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error "Cannot delete from empty deque")
      (let ((previous-node (item-prev (car (rear-ptr deque)))))
        (if (null? previous-node)
            (begin
              (set-rear-ptr! deque nil)
              (set-front-ptr! deque nil)
              deque)
            (begin
              (set-cdr! previous-node nil)
              (set-rear-ptr! deque previous-node)
              deque)))))

(define (print-deque deque)
  (map
    item-value
    (front-ptr deque)))

(define (display-deque deque)
  (display (print-deque deque))
  (display "; front: ")(display (front-deque deque))
  (display "; rear: ")(display (rear-deque deque))(newline))

(define d1 (make-deque))
(display (empty-deque? d1))(newline)
(display-deque (front-insert-deque! d1 'a))
(display-deque (front-insert-deque! d1 'b))
(display-deque (front-insert-deque! d1 'c))
(display-deque (rear-insert-deque! d1 'd))
(display-deque (rear-insert-deque! d1 'e))
(display-deque (rear-delete-deque! d1))
(display-deque (front-delete-deque! d1))
(display-deque (rear-delete-deque! d1))
(display-deque (rear-delete-deque! d1))
(display-deque (rear-delete-deque! d1))

(newline)

(define d2 (make-deque))
(display-deque (rear-insert-deque! d2 'a))
(display-deque (rear-insert-deque! d2 'b))
(display-deque (front-insert-deque! d2 'c))
(display-deque (rear-delete-deque! d2))
(display-deque (front-delete-deque! d2))
(display-deque (front-delete-deque! d2))
