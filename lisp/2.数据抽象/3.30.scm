(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? (car tree)))
          (cons (square (car tree)) (square-tree (cdr tree))))
        (else (cons (square-tree (car tree)) 
                   (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map
    (lambda (subtree)
      (if (pair? subtree)
        (square-tree2 subtree)
        (square subtree)))
  tree))


