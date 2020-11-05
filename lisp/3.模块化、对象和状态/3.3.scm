(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit input-pwd amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch input-pwd m)
    (if (eq? input-pwd password)
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else (error "Unknown request == MAKE-ACCOUNT" m)))
      (lambda (x) "Incorrect password")))
  dispatch)


;;;dispatch返回withdraw 或者 desposit 或者 lambda(x) “密码错误”

;;;make-account 作用在两个参数后 返回dispatch 


