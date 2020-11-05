
(define (make-monitored f)
  (let ((acc 0)) ;;;先定义(声明)再set
    (lambda (x)
      (cond 
        ((eq? x 'how-many-calls?) acc)
        ((eq? x 'reset-count) (set! acc 0))
        (else (begin (set! acc (+ acc 1))
                     (f x)))))))
