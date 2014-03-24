
(define (and-left  exp) (cadr exp))
(define (and-right exp) (cddr exp))
(define (make-and left-operand right-operand) (list 'and left-operand right-operand))

(define (eval-and exp env)
  (define (eval-and-cond conditions)
    (if (null? conditions)
	#f
	(let ((left (eval (car conditions) env)))
	  (if (true? left)
	      (if (null? (cdr conditions))
		  left
		  (eval-and-cond (cdr conditions)))
	      #f))))
  (eval-and-cond (cdr exp)))

(put 'eval 'and eval-and)

(define (or-left  exp) (cadr exp))
(define (or-right exp) (caddr exp))
(define (make-or left-operand right-operand) (list 'or left-operand right-operand))

(define (eval-or exp env)
  (define (eval-or-cond conditions)
    (display conditions)
    (if (null? conditions)
	#f
	(let ((left (eval (car conditions) env)))
	  (if (true? left)
	      left
	      (eval-or-cond (cdr conditions))))))
  (eval-or-cond (cdr exp)))

(put 'eval 'or eval-or)

; alternative
; ('and A B) -> (if A B A)
; ('or  A B) -> (if A A B)
; 1回余分に評価する？

