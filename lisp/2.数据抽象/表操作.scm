(define (list-ref items n) 
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;;;从0开始找下表为n的项

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;;;递归求表长


(define (length items)
  (define (length-iter a cnt)
    (if (null? a)
        cnt
        (length-iter (cdr a) (+ 1 cnt))))
  (length-iter items 0))

;;;迭代求表长


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;;;顺序append  





