(define (square x) (* x x))

(define (tree-map action tree)
    (map (lambda (subtree)
        (if (pair? subtree)
            (tree-map action subtree)
            (action subtree)))
    tree))

(define tree
  (list 1
        (list 2
              (list 3 4)
              5)
        (list 6 7)))

(display tree)(newline)
(display (tree-map square tree))(newline)
