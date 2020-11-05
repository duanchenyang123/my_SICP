(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
       (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
       (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ⟨procedures same-variable? and variable? from section 2.3.2⟩

  ;; representation of terms and term lists
  ⟨procedures adjoin-term … coeff from text below⟩

  (define (add-poly p1 p2) …)
  ⟨procedures used by add-poly⟩
  (define (mul-poly p1 p2) …)
  ⟨procedures used by mul-poly⟩

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p)) ;类型标志
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)


 (define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) 
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1) (rest-terms L2)))))))))
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term 
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))


;;;稀疏表示

(define (install-sparse-package)
  ;; 构造函数
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  ;; 选择函数
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  ;; 注册
  (define (tag x) (attach-tag 'sparse x))
  (put 'adjoin-term 'sparse
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse) rest-terms)
  'done)

(define (make-term order coeff) 
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term)) 


; 稠密表示
(define (install-dense-term-list) 
  ;; 构造函数 
  (define (adjoin-term term term-list)  
    (cond ((=zero? (coeff term)) term-list)  
          ((=equ? (order term) (length term-list)) (cons (coeff term) term-list))  
          (else (adjoin-term term (cons 0 term-list)))))  
  (define (first-term term-list)
    (make-term (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  ;; 注册 
  (define (tag x) (attach-tag 'dense x)) 
  (put 'adjoin-term 'dense
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term '(dense) first-term) 
  (put 'rest-term '(dense) rest-terms) 
  'done)


(define (adjoin-term term term-list)  
  ((get 'adjoin-term (type-tag term-list)) term (contents term-list))) 
(define (first-term term-list) (apply-generic 'first-term term-list)) 
(define (rest-term term-list) (apply-generic 'rest-term term-list)) 





