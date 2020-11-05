 (define (add-complex z1 z2)
  (make-from-real-imag 
   (+ (real-part z1) (real-part z2))
   (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag 
   (- (real-part z1) (real-part z2))
   (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang 
   (* (magnitude z1) (magnitude z2))
   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang 
   (/ (magnitude z1) (magnitude z2))
   (- (angle z1) (angle z2))))

(define (make-from-real-imag x y) (cons x y))
;; 由于采用直角坐标表示，所以如果传入模和幅角，要将其转换为实部和虚部

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

; 选择函数
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

;; 由于采用直角坐标表示，所以可以通过三角关系转换为对应的模和幅角
(define (magnitude z)
  (sqrt (+ (square (real-part z)) 
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))



;;极坐标
(define (make-from-mag-ang r a) (cons r a))
;; 由于采用极坐标表示，所以如果传入实部和虚部，要将其转换为模和幅角
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
; 选择函数
(define (magnitude z) (car z))
(define (angle z) (cdr z))
;; 由于采用极坐标表示，所以可以通过三角关系转换为对应的实部和虚部
(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))



;;;带标志的数据


; 将类型标志和数据内容组合起来
(define (attach-tag type-tag contents)
  (cons type-tag contents))

; 获得类型标志
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

; 获得数据内容
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))


; 判断是否为直角坐标表示
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
; 判断是否为极坐标表示
(define (polar? z)
  (eq? (type-tag z) 'polar))

; 直角坐标表示
;; 构造函数
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))
;; 选择函数
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
; 极坐标表示
;; 构造函数
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))
(define (make-from-real-imag-polar x y)
  (attach-tag 
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

;; 选择函数
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z)
  (* (magnitude-polar z) 
     (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) 
     (sin (angle-polar z))))

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown type:  REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rectangular (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z) (angle-rectangular (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

;(put <op> <type> <item>) ; 将<item>放入表中<op>行<type>列
;(get <op> <type>)        ; 从表中获得<op>行<type>列的操作

(define (install-rectangular-package)
  ;; 内部构造函数和选择函数
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; 插入表中的操作
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part) ; 列名与tag过程中插入的数据标识相同
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; 内部构造函数和选择函数
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; 插入表中的操作
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ; 根据传入数据获得类型标识符
    (let ((proc (get op type-tags))) ; 从二维表中获得对应的操作
      (if proc ; 如果存在操作
          (apply proc (map contents args)) ; 需要获取数据内容
          (error "No method for these types: APPLY-GENERIC"
            (list op type-tags))))))



 (define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (install-sum-deriv)
  ; 内部函数
  ;; 选择函数
  (define (addend variables) (car variables))
  (define (augend variables) (cadr variables))
  ;; 构造函数
  (define (make-sum a1 a2)
    (cond ((eq? a1 0) a2)
          ((eq? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  ;; 基本过程
  (define (sum-deriv variables var)
    (make-sum (deriv (addend variables) var)
              (deriv (augend variables) var)))
  ; 插入表格
  (put 'deriv '(+) sum-deriv)
  'done)

(define (install-product-deriv)
  ; 内部函数
  ;; 选择函数
  (define (multiplier variables) (car variables))
  (define (multiplicand variables) (cadr variables))
  ;; 构造函数
  (define (make-sum a1 a2)
    (cond ((eq? a1 0) a2)
          ((eq? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (eq? m1 0) (eq? m2 0)) 0)
          ((eq? m1 1) m2)
          ((eq? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  ;; 基本过程
  (define (product-deriv variables var)
    (make-sum (make-product (multiplier variables)
                            (deriv (multiplicand variables) var))
              (make-product (deriv (multiplier variables) var)
                            (multiplicand variables))))
  ; 插入表格
  (put 'deriv '(*) product-deriv)
  'done)




;;;信息传递
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))


(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

