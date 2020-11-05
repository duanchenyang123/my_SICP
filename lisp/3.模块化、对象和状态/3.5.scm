
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond
      ((= 0 trials-remaining) (/ trials-passed trials))
      ((experiment)
        (iter (- trials-remaining 1) (+ trials-passed 1)))
      (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (make-cesaro-test p x1 x2 y1 y2)
  (lambda ()
    (let ((x (if (> x1 x2) (random-in-range x2 x1) (random-in-range x1 x2)))
          (y (if (> y1 y2) (random-in-range y2 y1) (random-in-range y1 y2))))
      (p x y))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((width (abs (- x1 x2)))
        (height (abs (- y1 y2))))
    (* width height (monte-carlo trials (make-cesaro-test p x1 x2 y1 y2)))))


(define (in-circle? x y)
  (let ((px 5)
        (py 7)
        (radix 3))
    (< (+ (square (- x px)) (square (- y py))) (square radix))))


(estimate-integral in-circle? 2 8 4 10 1000.0)
