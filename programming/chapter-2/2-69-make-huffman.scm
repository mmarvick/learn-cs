(load "sect2-4-huffman/huffman.scm")

(define frequency-pairs
  (list '(B 2)
        '(A 4)
        '(C 1)
        '(D 1)))

(display frequency-pairs)(newline)
(display (make-leaf-set frequency-pairs))(newline)
(display (generate-huffman-tree frequency-pairs))(newline)
