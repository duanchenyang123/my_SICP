(define (enumerate-interval n)
    (if (< n 1)
      nil
      (append (enumerate-interval (- n 1))
              (list n))))


(define (unique-pairs n)
  (accumulate append nil 
    (map
      (lambda (i)
        (map (lambda (j) (list j i))
          (enumerate-interval (- i 1))))
      (enumerate-interval n))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (define (iter i)
    (if (< i n)
      (if (not (= 0 (remainder n i)))
        (iter (+ i 1))
        #f)
      #t))
  (iter 2))

(define (prime-sum-pairs n)
  (filter (lambda (p)
            (prime? (+ (car p) (cadr p))))
          (unique-pairs n)))


