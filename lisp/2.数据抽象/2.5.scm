(define (pow b n)
  (define (even? x) (= (remainder x 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (pow b (/ n 2))))
        (else (* b (pow b (- n 1))))))

(define (square x) (* x ))

(define (cons x y) (* (pow 2 x) (pow 3 y)))

(define (iter z count b) 
  (if (= (remainder z b) 0))
    (iter (/ z b (+ 1 count) divider)
    count))

(define (car z) (iter z 0 2))
(define (cdr z) (iter z 0 3))
