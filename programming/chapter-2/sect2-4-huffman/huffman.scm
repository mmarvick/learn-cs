(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? node) (eq? (car node) 'leaf))
(define leaf-symbol cadr)
(define leaf-weight caddr)

(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))
(define tree-left car)
(define tree-right cadr)
(define tree-symbols caddr)
(define tree-weight cadddr)

(define (weight node)
  (if (leaf? node)
    (leaf-weight node)
    (tree-weight node)))

(define (symbols node)
  (if (leaf? node)
    (list (leaf-symbol node))
    (tree-symbols node)))

(define (decode bits tree)
  (define (decide-subtree bit tree)
    (cond ((= bit 0)
            (tree-left tree))
          ((= bit 1)
            (tree-right tree))
          (else
            error "INVALID BIT - " bit)))
  (define (decode-iter bits tree-current)
    (if (null? bits)
      '()
      (let ((subtree (decide-subtree (car bits) tree-current))
            (remaining-bits (cdr bits)))
        (if (leaf? subtree)
          (cons (leaf-symbol subtree) (decode-iter remaining-bits tree))
          (decode-iter remaining-bits subtree)))))
  (decode-iter bits tree))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-iter node encoding)
    (if (leaf? node)
      (if (eq? symbol (leaf-symbol node)) encoding '())
      (let ((left-result (encode-symbol-iter (tree-left node) (append encoding (list 0)))))
        (if (not (null? left-result))
          left-result
          (let ((right-result (encode-symbol-iter (tree-right node) (append encoding (list 1)))))
            right-result)))))
  (let ((encoding (encode-symbol-iter tree '())))
    (if (null? encoding)
      (error "CANNOT ENCODE SYMBOL - " symbol)
      encoding)))

(define (generate-huffman-tree pairs)
  (define (successive-merge leaf-set)
    (cond ((null? leaf-set)
            '())
          ((null? (cdr leaf-set))
            (car leaf-set))
          (else
            (successive-merge
              (adjoin-set
                (make-code-tree (car leaf-set) (cadr leaf-set))
                (cddr leaf-set))))))
  (successive-merge (make-leaf-set pairs)))

; add to a set of leaf nodes, ordering from lowest to highest weight
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
          (cons x set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

; make a set of leaf nodes from frequency pairs
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (adjoin-set (make-leaf (caar pairs) (cadar pairs))
                (make-leaf-set (cdr pairs)))))
