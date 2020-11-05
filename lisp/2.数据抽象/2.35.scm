(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial ;递归的最后再加初始值 别急
      (op (car sequence) (accumulate op initial (cdr sequence)))))

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


(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (x) 1) (fringe t))))

 (define (count-leaves t)
    (accumulate (lambda (x y) (+ 1 y))
                0
                (fringe t)))
