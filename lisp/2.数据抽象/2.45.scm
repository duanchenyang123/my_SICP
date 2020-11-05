(define (split l r)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split l r) painter (- n 1))))
        (l painter (r smaller smaller))))))

