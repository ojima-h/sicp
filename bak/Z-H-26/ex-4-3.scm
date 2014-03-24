(define dispatch-table (make-hash-table 'equal?))
(define (get op type) (hash-table-get dispatch-table (list op type)))
(define (put op type proc) (hash-table-put! dispatch-table (list op type) proc))
(define (op-installed? op type) (hash-table-exists? dispatch-table (list op type)))

(define (eval exp env)
  (cond
   ; atomic expression
   ((self-evaluating? exp) exp)
   ((variable? exp) (lookup-variable-value exp env))
   ((quoted? exp) (text-of-quotation exp))

   ; special forms
   ((op-installed? 'eval (car exp)) ((get 'eval (car exp)) exp env))

   ; normal procedure
   ((application? exp)
    (apply (eval (operator exp) env)
	   (list-of-values (operands exp) env)))
   (else
    (error "Unknown expression type -- EVAL" exp))))

(put 'eval 'set!   eval-assignment)
(put 'eval 'define eval-definition)
(put 'eval 'if     eval-if)
(put 'eval 'lambda (lambda (exp env)
		     (make-procedure (lambda-parameters exp)
				     (lambda-body exp)
				     env)))
(put 'eval 'begin (lambda (exp env)
		    (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond  (lambda (exp env)
		    (eval (cond->if exp) env)))
