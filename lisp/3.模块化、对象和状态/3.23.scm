(define (set-front-ptr! deque p)
  (set-car! deque p))
(define (set-rear-ptr! deque p) (set-cdr! deque p))

(define (make-deque) (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (empty-deque? deque) 
  (or (eq? (front-ptr deque) '())
      (eq? (rear-ptr deque) '() )))

(define (front-deque deque) (front-ptr deque))
(define (rear-deque deque) (rear-ptr deque))

;;;两头的插入和删除

(define (front-insert-deque! deque item)
  (let ((data-pair (cons item '())))
    (if (empty-deque? deque)
      (let ((ptr-pair (cons data-pair '())));;;这就是骨架的元素
        (set-front-ptr! deque ptr-pair)
        (set-rear-ptr! deque ptr-pair)
        deque)
      (let ((first-data-pair (car (front-deque deque)));;先保存旧头
            (ptr-pair (cons data-pair (front-deque deque))));;;骨架元素pair
        (set-cdr! first-data-pair ptr-pair) ;;;旧头元素的cdr指针指新的骨架元素  反指
        (set-front-ptr! deque ptr-pair);;;deque的头部指针指向新的骨架元素头
        deque))))


;;;头出队
(define (front-delete-deque! deque)
  (if (empty-deque? deque)
    (error "DELETE on empty deque")
    (let ((first-ptr (front-deque deque)))
      (let ((second-ptr (cdr first-ptr)))
        (if (null? second-ptr)
          (begin
            (set-front-ptr! deque '())
            deque)
          (begin
            (set-cdr! (car second-ptr) '())
            (set-front-ptr! deque second-ptr)
            deque))))))



;;;尾插
(define (rear-insert-deque! deque item)
  (let ((data-pair (cons item '())))
    (if (empty-deque? deque)
      (let ((ptr-pair (cons data-pair '())))
        (set-front-ptr! deque ptr-pair)
        (set-rear-ptr! deque ptr-pair)
        deque)
      (let ((ptr-pair (cons data-pair '())));;;先用骨架pair包上
        (set-cdr! data-pair (rear-ptr deque));;;在把数据pair的cdr指针指向deque的rear指针指向的那个（骨架元素）
        (set-rear-ptr! deque ptr-pair) ;;;更新deque的rear指针
        deque))))
;;;不用保存旧的信息

;;;尾部出
(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
    (error "DELETE on empty deque")
    (let ((last-ptr (rear-deque deque)))
      (let ((second-ptr (cdar last-ptr)));;;我感觉这里是cdar
        (if (null? second-ptr)
          (begin
            (set-rear-ptr! deque '())
            deque)
          (begin
            (set-cdr! second-ptr '())
            (set-rear-ptr! deque second-ptr)
            deque))))))

(define (print-deque deque)
  (define (iter l)
      (if (null? l)
            '()
             (cons (caar l) (iter (cdr l)))))
   (iter (front-ptr deque)))

