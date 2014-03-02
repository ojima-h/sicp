
(define (and-left  exp) (cadr exp))
(define (and-right exp) (caddr exp))
(define (make-and left-operand right-operand) (list 'and left-operand right-operand))

(define (eval-and exp env)
  (let ((left-value (eval (and-left exp) env)))
    (if (true? left-value)
	(eval (and-right exp) env)
	left-value)))

(put 'eval 'and eval-and)

(define (or-left  exp) (cadr exp))
(define (or-right exp) (caddr exp))
(define (make-or left-operand right-operand) (list 'or left-operand right-operand))

(define (eval-or exp env)
  (let ((left-value (eval (or-left exp) env)))
    (if (true? left-value)
	left-value
	(eval (or-right exp) env))))

(put 'eval 'or eval-or)

; alternative
; ('and A B) -> (if A B A)
; ('or  A B) -> (if A A B)
; 1回余分に評価する？

