(load"2.36.scm")

(define nil '())

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda(row) (dot-product v row)) m))

(define (transpose mat)
	(accumulate-n cons nil mat))

(define (matrix-*-matrix m n) 
  (let ((clos (transpose n)))
    (map (lambda(row-of-mat) (matrix-*-vector clos row-of-mat)) m)))


