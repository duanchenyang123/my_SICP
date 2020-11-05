(define (equal? seq1 seq2)
  (cond
    ((and (null? seq1) (null? seq2)) #t)
    ((or (null? seq1) (null? seq2)) #f)
    (else (and
            (eq? (car seq1) (car seq2))
            (equal? (cdr seq1) (cdr seq2))))))
