(define (encode message tree)
  (if (null? message)
    '() 
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (exist? char symbols)
  (if (null? symbols)
    #f
    (or (eq? char (car symbols))
        (exist? char(cdr symbols)))))

(define (encode-symbol char tree)
  (cond 
    ((null? tree) (error "no such char" char))
    (leaf? tree) '())
    (else 
      (let ((lb (left-branch tree))
           (rb (right-branch tree)))
        (let ((ls (symbols lb))
              (rs (symbols rb)))
          (cond
            ((exist? char ls)
              (cons 0 (encode-symbol char lb)))
            ((exist? char rs)
             (cons 1 (encode-symbol char rb)))
            (else (error "no such2 char" char)))))))

