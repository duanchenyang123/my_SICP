(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer
                  (list (square (car things)))))))
  (iter items nil))
(square-list 1-4)

;;;每次把平方后的 append到anser列表后
