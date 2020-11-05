; 构造函数：返回一个空队列
(make-queue)
; 选择函数
(empty-queue? <queue>)
(front-queue <queue>)
; 改变函数
(insert-queue! <queue> <item>)
(delete-queue! <queue>)
; 构造函数
(define (make-queue) (cons '() '()))
; 选择函数
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;;;队尾插入 队头出去


; 构造函数
(define (make-queue) (cons '() '()))
; 选择函数
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;;;入队时要改尾部指针
(define (set-rear-ptr! queue item) (set-cdr! queue item))
;;;出队后要改头部指针
(define (set-front-ptr! queue item) (set-car! queue item))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '()))) ;建立新的序对
    (cond ((empty-queue? queue)
              (set-front-ptr! queue new-pair)
              (set-rear-ptr! queue new-pair)
               queue)
          (else (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                 queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
           (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
               queue)))

;;;打印 2.21
(define (print-queue queue) (car queue))

