
(define (horner-eval x coefficitent-sequences)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)));;;higher-terms就是更高的项 们  也就是集合
              0
              coefficitent-sequences))
