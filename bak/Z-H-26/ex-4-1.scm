(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
	(let
	    ((first-value (eval (first-operand exps) env)))
	  (cons first-value rest-values)))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
	(cons first-value (list-of-values (rest-operands exps) env)))))

