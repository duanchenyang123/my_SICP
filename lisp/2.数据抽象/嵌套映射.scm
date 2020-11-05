(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial ;递归的最后再加初始值 别急
      (op (car sequence) (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cdr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
              (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate0interval 1 (- i 1))))
                  (enumerate0interval 1 n)))))

