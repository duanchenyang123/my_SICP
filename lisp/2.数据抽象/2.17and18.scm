(define (last-pair l) 
  (if (null? (cdr l)
      l
      (last-pair (cdr l)))))

;;;2.18
(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l)) (list(car l)))))


