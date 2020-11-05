(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))) ;;;根据传入数据获得类型标志
    (let ((proc (get op type-tags)))  ;;;查表 查对应的操作
      (if proc 
        (apply proc (map contents args))
        (error "No method for these types: apply-generic" (list op type-tags))))))

;;;该函数根据op参数选择表格中对应的行，然后根据args中的数据类型表来在表格中选择对应的操作，然后将操作应用到数据内容上。

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number))
  (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
  
  (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
  'done)

(define (attach-tag type-tag contents)
  (if (eq? 'scheme-number type-tag)
      contents
      (cons type-tag contents)))

(define (type-tag datum) 
  (cond ((number? datum) 'scheme-number) 
        ((pair? datum) (car datum)) 
        (else (error "Wrong datum -- TYPE-TAG" datum)))) 

(define (contents datum) 
  (cond ((number? datum) datum) 
        ((pair? datum) (cdr datum)) 
        (else (error "Wrong datum -- CONTENTS" datum)))) 

;;;不给scheme number赋标志

;;;;有理数运算

(define (install-rational-package)
  ;;;内部过程
  ;;构造函数
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  ;;;选择函数
  (define (numer rat) (car rat))
  (define (denom rat) (cdr rat))
  ;;;基本过程
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (numer y) (denom x))))
  ;;注册
  ;;添加标签
  (define (tag x) (cons 'rational x))
  ;;;注册因为这是内部函数 所以就能匹配上

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)




 ; 复数操作
;; 首先定义不同表示上的通用操作real-part、imag-part、magnitude和angle，对应表格为
;;           rectangular   polar
;;real-part
;;imag-part
;;magnitude
;;angle
;; 为了避免名字冲突，用两个函数来分别注册
(define (install-rectangular-package)
  ;; 构造函数
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; 选择函数
  (define (real-part x) (car x))
  (define (imag-part x) (cdr x))
  (define (magnitude x)
    (sqrt (+ (square (real-part x))
             (square (imag-part x)))))
  (define (angle x)
    (atan (imag-part x) (real-part x)))
  ;; 注册
  (define (tag x) (cons 'rectangular x)) ;用rectangular表示直角坐标表示的负数
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  'done)
(define (install-polar-package)
  ;; 构造函数
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; 选择函数
  (define (magnitude x) (car x))
  (define (angle x) (cdr x))
  (define (real-part x)
    (* (magnitude x) (cos (angle x))))
  (define (imag-part x)
    (* (magnitude x) (sin (angle x))))
  ;; 注册
  (define (tag x) (cons 'polar x)) ;用polar表示直角坐标表示的负数
  (put 'make-from-mag-ang 'polar
       (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  'done)
(install-rectangular-package)
(install-polar-package)
(define (magnitude x)
  (apply-generic 'magnitude x))
(define (angle x)
  (apply-generic 'angle x))
(define (real-part x)
  (apply-generic 'real-part x))
(define (imag-part x)
  (apply-generic 'imag-part x))

;; 注册在复数上的算数运算
(define (install-complex-package)
  ;;; 构造函数 可以直接获得注册好的函数
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)) ;这里还没加上标签，只能用get
  (define (make-from-mag-ang x y)
    ((get 'make-from-mag-ang 'polar) x y))
  ;;; 基本过程
  (define (add-complex x y)
    (make-from-real-imag (+ (real-part x) (real-part y))
                         (+ (imag-part x) (imag-part y))))
  (define (sub-complex x y)
    (make-from-real-imag (- (real-part x) (real-part y))
                         (- (imag-part x) (imag-part y))))
  (define (mul-complex x y)
    (make-from-mag-ang (* (magnitude x) (magnitude y))
                       (* (angle x) (angle y))))
  (define (div-complex x y)
    (make-from-mag-ang (/ (magnitude x) (magnitude y))
                       (/ (angle x) (angle y))))
  ;;; 注册
  (define (tag x) (cons 'complex x))
  (put 'add '(complex complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex)
       (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag make-from-mag-ang x y)))
  )

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))


(define (scheme-number->complex x)
  (make-complex-from-real-imag (content n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))










