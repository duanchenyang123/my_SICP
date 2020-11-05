(define (make-rand seed)
  (define (dispatch m)
    (cond
      ((eq? m 'generate)
        (let ((new-value (rand-update seed)))
          (reset! seed new-value)
          new-value));;;更新之后输出
      ((eq? m 'reset)
        (lambda (newSeed))
          (reset! seed newSeed)
          (rand-update seed));;;替换为输入值之后更新
      (else
        (error "Wrong method: " m))))
  dispatch)

