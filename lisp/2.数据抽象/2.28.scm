(define (fringe tree)
  (cond 
    ((null? (cdr tree))
      (if (pair? (car tree))
          (fringe (car tree))
          tree))
    ((not (pair? (car tree)))
      (cons (car tree) (fringe (cdr tree))))
    (else (appened (fringe (car tree))
                   (fringe (cdr tree))))))

