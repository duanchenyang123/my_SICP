(define (same-parity flag . others)
  (define (iter is_even l)
    (cond ((null? l) nil)
          ((even? (car l))
            (if is_even 
              (cons (car l) (iter is_even (cdr l)))
              (iter is_even (cdr l))))
           (else 
             (if is_even
               (iter is_even (cdr l))
               (cons (car l) (iter is_even (cdr l)))))))
  (let ((is_even (even? flag)))
    (display is_even)
    (cons flag (iter is_even others))))


