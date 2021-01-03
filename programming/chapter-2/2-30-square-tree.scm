(define tree
  (list 1
        (list 2
              (list 3 4)
              5)
        (list 6 7)))

(define (square-tree-1 tree factor)
  (cond ((null? tree)
          '())
        ((pair? tree)
          (cons (square-tree-1 (car tree) factor)
                (square-tree-1 (cdr tree) factor)))
        (else (* tree factor))))

(define (square-tree-2 tree factor)
  (map (lambda (subtree)
    (if (pair? subtree)
          (square-tree-2 subtree factor)
          (* subtree factor)))
    tree))

(display tree)(newline)
(display (square-tree-1 tree 10))(newline)
(display (square-tree-2 tree 10))(newline)
