(define (traverse-environment var env operator fallback)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)

      (let ((frame (first-frame env)))
	(define (scan vars vals)
	  (cond ((null? vars)
		 (fallback frame))
		((eq? var (car vars))
		 (operator vals))
		(else (scan (cdr vars) (cdr vals)))))
	(scan (frame-variables frame)
	      (frame-values frame)))))


(define (lookup-variable-value var env)
  (traverse-environment var env
			(lambda (vals) (car vals))
			(lambda (frame)
			  (lookup-variable-value var (enclosing-environment env)))))

(define (set-variable-value! var val env)
  (traverse-environment var env
			(lambda (vals) (set-car! vals val))
			(lambda (frame)
			  (set-variable-value! var val (enclosing-environment env)))))

(define (define-variable! var val env)
  (traverse-environment var env
			(lambda (vals) (set-car! vals val))
			(lambda (frame)
			  (add-binding-to-frame! var val frame))))


;; test
(let ()
  (define sample-env
    '(
      ((a b c) . (1 2 3))
      ((p q r) . (5 6 7))))

  (display (lookup-variable-value 'q sample-env)) (newline)

  (set-variable-value! 'p 11 sample-env)
  (display sample-env) (newline)

  (define-variable! 'd 12 sample-env)
  (display sample-env) (newline)

  (unset-variable-value! 'q sample-env)

)
