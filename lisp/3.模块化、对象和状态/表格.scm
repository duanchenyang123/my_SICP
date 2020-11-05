(define (make-table) (list '*table*)) ;只包含一个表头

(define (lookup key table)
  (let ((record (assoc key (cdr table)))) ; (cdr table)可以忽略表头
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) ; 判断记录的标识是否和key相同car是一个pair caar是key 
         (car records)) ; 相同的话就返回记录的序对
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value) ; 如果表格已存在该记录，就直接修改记录的值
        (set-cdr! table (cons (cons key value) (cdr table))))) 
         ; 如果不存在，就需要在表头插入新的记录(cons key value),没有顺序
  'ok)

;;;二维表格
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table)))) ; 查看在哪一行
    (if subtable
        (let ((record (assoc key-2 (cdr subtable)))) ; 查看在当前行是否包含该记录
          (if record
              (cdr record)
              false))
        false)))

;;;插入
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table)))) ; 判断是否存在对应行
    (if subtable
        (let ((record (assoc key-2 (cdr subtable)))) ; 判断在当前行是否存在对应标识的记录
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
        ; 当不存在对应行时，通过(cons key-2 value)创建记录
        ; 用(cons (list key-1 (cons key-2 value)))创建新的行
        (set-cdr! table (cons (list key-1 (cons key-2 value)) (cdr table)))))
  'ok)

;;;表格对象 有插入和查找方法

(define (make-table)
  (let ((local-table (list '*table*))) ; 初始化一个局部表格，该表格就存在于该过程内
    (define (lookup key-1 key-2) ; 此时表格为局部状态，无需将其作为参数
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table 
                      (cons (list key-1 (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
