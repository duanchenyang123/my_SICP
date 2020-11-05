(define nil '())
(define already-seen '())
(define (seen? x)
  (define (iter already-seen)
    (if (null? already-seen)
      #f
      (or (eq? x (car already-seen))
          (iter (cdr already-seen)))))
  (iter already-seen))

(define (count-pairs x)
  (if (not (pair? x))
    0
    (if (seen? x) 
      0
      (begin
        (set! already-seen (cons x already-seen))
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))))
