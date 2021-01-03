(load "sect2-4-huffman/huffman.scm")

(define frequency-pairs
  (list '(A 2)
        '(BOOM 1)
        '(GET 2)
        '(JOB 2)
        '(NA 16)
        '(SHA 3)
        '(YIP 9)
        '(WAH 1)))

(define sample-message
    '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(let* ((huffman-tree (generate-huffman-tree frequency-pairs))
       (encoded-message (encode sample-message huffman-tree)))
            (display "Huffman tree:")(newline)
            (display huffman-tree)(newline)
            (display "Encoded message:")(newline)
            (display encoded-message)(newline)
            (display "Original number of characters: ")(display (length sample-message))(newline)
            ; a fixed-length alphabet for 8 symbols requires 4 bits per symbol (log_2(8))
            (display "Unencoded length (bits): ")(display (* 4 (length sample-message)))(newline)
            (display "Encoded Length (bits):   ")(display (length encoded-message))(newline))
