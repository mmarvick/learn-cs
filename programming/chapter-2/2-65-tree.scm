(define nil '())
(define tree1 (list 7                               ; (1 3 5 7 9 11)
                    (list 3
                          (list 1 nil nil)
                          (list 5 nil nil))
                    (list 9
                          nil
                          (list 11 nil nil))))
(define tree2 (list 8                               ; (1 3 5 8 10 12 14 16)
                    (list 3
                          (list 1 nil nil)
                          (list 5 nil nil))
                    (list 10
                          nil
                          (list 14
                            (list 12 nil nil)
                            (list 16 nil nil)))))

(define (make-tree elem left right) (list elem left right))
(define tree-elem car)
(define tree-left cadr)
(define tree-right caddr)

(define (tree->list tree)
    (if (null? tree)
        nil
        (append (tree->list (tree-left tree))
                (list (tree-elem tree))
                (tree->list (tree-right tree)))))

(define (list->tree l)
    (car (partial-tree l (length l))))

(define (partial-tree elem n)
  (if (= n 0)
      (cons nil elem)
      (let* ((right-size (quotient n 2))
             (left-size (- n right-size 1))
             (left-result (partial-tree elem left-size))
             (left-tree (car left-result))
             (remaining-elem (cdr left-result))
             (elem (car remaining-elem))
             (right-elem (cdr remaining-elem))
             (right-result (partial-tree right-elem right-size))
             (right-tree (car right-result))
             (unused-elem (cdr right-result))
             (tree (make-tree elem left-tree right-tree)))
                (cons tree unused-elem))))

(define (union-set t1 t2)
  (define (union-ordered-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else (let ((x1 (car l1))
                      (x2 (car l2)))
            (cond ((< x1 x2)
                      (cons x1 (union-ordered-list (cdr l1) l2)))
                  ((> x1 x2)
                      (cons x2 (union-ordered-list l1 (cdr l2))))
                  (else
                      (cons x1 (union-ordered-list (cdr l1) (cdr l2)))))))))
  (list->tree
    (union-ordered-list
      (tree->list t1)
      (tree->list t2))))

(define (intersection-set t1 t2)
  (define (union-ordered-list l1 l2)
    (if (or (null? l1) (null? l2))
        nil
        (let ((x1 (car l1))
              (x2 (car l2)))
          (cond ((< x1 x2)
                    (union-ordered-list (cdr l1) l2))
                ((> x1 x2)
                    (union-ordered-list l1 (cdr l2)))
                ((= x1 x2)
                    (cons x1
                          (union-ordered-list (cdr l1) (cdr l2))))))))
  (list->tree
    (union-ordered-list
      (tree->list t1)
      (tree->list t2))))

(display (tree->list tree1))(newline)
(display (tree->list tree2))(newline)
(display (list->tree (list 1 2 3 4 5 6 7 8 9 10)))(newline)
(display (list->tree (tree->list tree1)))(newline)
(display (union-set tree1 tree2))(newline)
(display (intersection-set tree1 tree2))(newline)
