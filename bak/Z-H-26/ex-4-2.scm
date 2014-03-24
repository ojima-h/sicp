;;define や set! などの syntax keyword や special form を
;;普通の関数として認識してしまうから。

(define (eval-tmp exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))

        ((application-tmp? exp)
	 (apply (eval (operator-tmp exp) env)
		(list-of-values (operands-tmp exp) env))
	 )

        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
(define (application-tmp? exp) (tagged-list? exp 'call))
(define (operator-tmp exp) (cadr exp))
(define (operands-tmp exp) (cddr exp))
