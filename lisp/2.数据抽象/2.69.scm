(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))




(define (successive-merge leaf-sets)
  (if (= 1 (length leaf-sets))
    (car leaf-sets)
    (successive-merge 
      (arrange (make-code-tree (car leaf-sets) (cadr leaf-sets))
               (cddr leaf-sets)))))



(define (arrange tree leaf-sets)
  (cond
    ;第一次直接返回了tree，其实arrange应该返回个list，调了好久😂
    ((null? leaf-sets) (list tree))
    (else 
      (let ((tw (weight tree))
            (lw (weight (car leaf-sets))))
        (if (< tw lw)
          (cons tree leaf-sets)
          (cons (car leaf-sets)
                (arrange tree (cdr leaf-sets))))))))
