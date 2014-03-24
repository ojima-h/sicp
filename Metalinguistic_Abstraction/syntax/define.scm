(define-module syntax.define
  (use syntax)
  (use core)

  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  (define (definition-variable exp)
    (if (symbol? (cadr exp))
	(cadr exp)
	(caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
	(caddr exp)
	(make-lambda (cdadr exp)   ; formal parameters
		     (cddr exp)))) ; body

  (install-syntax 'define eval-definition)
)  
