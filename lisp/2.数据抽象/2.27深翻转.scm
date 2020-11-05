(define (deep-reverse items)
  (cond
    ((null? items) nil)
    ((not (pair? items)) items)
    (else (appened (deep-reverse (cdr items))
                   (list (deep-reverse (car items)))))))
