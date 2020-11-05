(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* factor (car items))
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (* x x) (list 1 2 3 4)))


