(define (make-sum . args)
  (define (iter res l)
    (if (null? l)
      res
      (let ((first (car l)))
        (if (=number? first 0)
          (iter res (cdr l))
          (iter (append res (list (car l)))
                (cdr l))))))
  (iter '(+) args))

(define (append exp)
  (cadr exp))

(define (augend exp)
  (define (iter result l)
    (if (null? l)
      result
      (iter (append result (list (car l)))
            (cdr l))))
  (if (null? (cdddr expp))
    (caddr exp)
    (let ((rest (cddr exp)))
      (iter '(+) rest))))

(define (make-product . agrs)
  (define (iter res l)
    (if (null? l)
      res
      (iter (append res (list (car l))) (cdr l))))
  (iter '(*) args))

(define (multipler exp)
  (cadr exp))

(define (multiplicant exp)
  (define (iter result l)
    (if (null? l)
      result
      (iter (append result (list (car l))) (cdr l))))
  (if (null? (cdddr exp))
    (caddr exp)
    (let ((rest (cddr exp)))
      (iter '(*) rest))))



(define a1 addend)
(define a2 augend)

(define m1 multipler)
(define m2 multiplicant)


