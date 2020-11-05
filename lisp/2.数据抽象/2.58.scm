(define (make_sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list a1 '+ a2))))

(define (make_product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list m1 '* m2))))


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))  
(define (multiplicand p) (caddr p))




;;;b

(define (operation expr) 
 (if (memq '+ expr) 
      '+ 
      '*))

(define (sum? expr)
  (eq? '+ (operation expr)))

(define (addend expr)
  (define (iter expr result)
    (if (eq? (car expr) '+)
      result
      (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '() )))
    (if (= (length result) 1)
      (car result)
      result)))

 (define (augend expr)
   (let ((result (cdr (memq '+ expr))))
     (if (= (length result) 1)
       (car result)
       result)))


(define (product? expr)
  (eq? '* (operation expr)))


(define (multiplier expr) 
  (car expr))
(define (multiplicand expr)
  (if (= 1 (length expr))
    (car expr)
    (cddr expr)))







