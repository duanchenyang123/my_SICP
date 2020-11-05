(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial ;递归的最后再加初始值 别急
      (op (car sequence) (accumulate op initial (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              sequence))
(map (lambda(x) (+ 1 x)) (list 1 2 3 4))              
;Value: (2 3 4 5)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append (list 1 2) (list 3 4))
;Value: (1 2 3 4)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
(length (list 1 2 3 4))
;Value: 4
