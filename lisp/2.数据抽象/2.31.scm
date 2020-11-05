 (define (tree-map proc tree)
  (map
      (lambda (subtree)
        (if (pair? subtree)
          (tree-map proc subtree)
          (proc subtree)))
    tree))i


(define (square-tree tree)
	(tree-map square tree))


