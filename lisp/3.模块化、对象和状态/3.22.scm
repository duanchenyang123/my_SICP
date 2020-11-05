;;;类似用面向对象封装队列

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    ; inner helper operations
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    ; selectors
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue")
        front-ptr))
    (define (empty-queue?)
      (null? front-ptr))
    ; mutators
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty-queue? queue)
          (begin
            (set-front-ptr! new-pair)
            (set-rear-ptr! new-pair))
          (begin
            (set-cdr! rear-ptr new-pair)
            (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (if (empty-queue?)
        (error "DELETE called with an empty queue")
        (set-front-ptr (cdr front-ptr))))
    (define (dispatch m)
      (cond
        ((eq? m 'empty-queue?) empty-queue?)
        ((eq? m 'front-queue) front-queue)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        (else  (error "Unknown operation" m))))
    dispatch))
