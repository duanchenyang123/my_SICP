
(define (element-of-set? e set)
  (cond
    ((or (null? set) (> (car set) e)) #f)
    ((= (car set) e) #t)
    (else 
      (element-of-set? e (cdr set)))))

(element-of-set? 4 '(1 3 4 5 6))

(define (adjoin-set a set)
  (cond
    ((null? set) (cons a set))
    ((= (car set) a) set)
    ((> (car set) a)
      (cons a set))
    (else 
      (cons (car set)
        (adjoin-set a (cdr set))))))
