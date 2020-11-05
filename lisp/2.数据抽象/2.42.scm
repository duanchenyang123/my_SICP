(define (enumerate-interval n)
  (if (< n 1)
      nil
      (append (enumerate-interval (- n 1))
              (list n))))

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions)
          (safe? k positions))
        (flatmap ;;;flatmap就是把倒数第二层的括号去掉
          (lambda (rest-of-queens)
            (map
              (lambda (new-row)
                (adjoin-position new-row k rest-of-queens))
               (enumerate-interval board-size)))
          (queens-cols (- k 1)))        )))
  (queens-cols board-size))

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (ok? v pos offset)
  (cond 
    ((null? pos) #t)
    ((and (not (= v (car pos)))
          (not (= v (- (car pos) offset)))
          (not (= v (+ (car pos) offset))));;;先比新放的左边那一列的皇后能不能吃
      (ok? v (cdr pos) (+ 1 offset)));;;再前一列也不能吃 
    (else #f)))

(define (safe? k positions)
  (let ((rev_positions (reverse positions)))
    (ok? (car rev_positions) (cdr rev_positions) 1)))
;;;把摆放排列反过来 第一个数是最后新放的。


