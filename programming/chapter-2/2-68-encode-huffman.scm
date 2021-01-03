(load "sect2-4-huffman/huffman.scm")

(define sample-encoded-message '(a d a b b c a))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(display (encode sample-encoded-message sample-tree))(newline)
