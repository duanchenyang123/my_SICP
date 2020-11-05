(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-square (car tree))
                 (sum-odd-square (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;;;过滤一个序列

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;;;累计(折叠)

(define (accumulate op initial sequence) 
  (if (null? sequence) 
      initial ;递归的最后再加初始值 别急
      (op (car sequence) (accumulate op initial (cdr sequence)))))

;;;枚举一个序列
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ 1 low) high))))

;;;枚举一棵树的所有树叶 练习2.28
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-square tree)
  (accumulate +
              0
              (map square (filter odd? (enumerate-tree tree)))))

;;;even的fib数的列表
(define (even-fibs n)
  (accumulate cons 
              nil
              (filter even? (map fib (enumerate-interval 0 n)))))

;;;fib数的平方的列表
(define (list-fib-squares n)
  (accumulate cons 
              nil
              (map square (map fib (enumerate-interval 0 n)))))

;;;一个序列的odd的数的平方的连乘
(define (prouct-of-square-of-odd-elements sequence)
  (accumulate *
              1
              (map square (filter odd? sequence))))




