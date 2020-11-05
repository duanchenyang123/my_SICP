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

(define (equal-rat? x y) 
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))

(car x) ;;; 1
(cdr x) ;;; 2

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)
;;; 1/2

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
;;; 5/6

(print-rat (add-rat one-third one-third))
;;;6 / 9

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;;;习题2.1
(define (gcd x y)
  (if (= y 0)
    x
    (gcd y (remainder x y))))
(define (make-rat x y)
  (let ((g (gcd x y)))
    (if (< (x / y) 0)
      (cons (/ (- x) g) (/ y g))
      (cons (/ x g) (/ y g)))))


;;;打印时在取值时化简
(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x) 
  (let ((g (gcd (car x) (cdr x))))
    (/ ((cdr x) g))))

;;;2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-rect p1 p2) 
  (cons p1 p2))

(define (width r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (x-point p1) (x-point p2))))

(define (height r)
  (let ((p1 (car r))
        (p2 (cdr r)))
    (abs (y-point p1) (y-point p2))))

(define (area r) 
  (* (height r) (width r)))

(define (perimeter r)
  (* 2 (+ (width r) (height r))))

(define (make-rect a w h)
  (cons a (cons w h)))

(define (rect-width r)
  (car (cdr r)))

(define (rect-height r)
  (cdr (cdr r)))

(define (cons x y)
  (define (dispath m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Arguent not 0 or 1 --cons" m))))
 dispath)

(define (car z) (z 0))
(define (cdr z) (z 1))
;;;这么看 数据好像是过程 

;;;2.4


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda(p q) p)))

(define (cdr z)
  (z (lambda(p q) q)))

