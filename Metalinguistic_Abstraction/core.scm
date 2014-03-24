(define-module core
  (export eval)
  (export apply)

  (use syntax)

  (define (eval exp env)
    (cond
     ; atomic expression
     ((self-evaluating? exp) exp)
     ((variable? exp) (lookup-variable-value exp env))
     ((quoted? exp) (text-of-quotation exp))

     ; special forms
     ((installed-syntax? (car exp)) (eval-syntax (car exp) exp env))

     ; normal procedure
     ((application? exp)
      (apply (eval (operator exp) env)
	     (list-of-values (operands exp) env)))
     (else
      (error "Unknown expression type -- EVAL" exp))))

  (define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
	   (apply-primitive-procedure procedure arguments))
	  ((compound-procedure? procedure)
	   (eval-sequence
	    (procedure-body procedure)
	    (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
	  (else
	   (error
	    "Unknown procedure type -- APPLY" procedure))))

  (define (list-of-values exps env)
    (if (no-operands? exps)
	'()
	(let ((first-value (eval (first-operand exps) env)))
	  (cons first-value (list-of-values (rest-operands exps) env)))))
  (define (self-evaluating? exp)
    (cond ((number? exp) true)
	  ((string? exp) true)
	  (else false)))
  (define (variable? exp) (symbol? exp))
  (define (quoted? exp)
    (tagged-list? exp 'quote))
  (define (text-of-quotation exp) (cadr exp))

  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops))
)
