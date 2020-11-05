(define (make-accumulator acc)
  (lambda (x) 
    (set! acc (+ acc x))
    acc))


