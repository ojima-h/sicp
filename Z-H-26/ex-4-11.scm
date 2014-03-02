(let ()
  (define (make-frame bindings) (list bindings))
  (define (frame-bindigns frame) (car bindings))
  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons (cons var val) frame)))

  (define (extend-environment bindings base-env)
    (cons (make-frame bindings) base-env))

  (define (lookup-variable-value var env)
    (define (env-loop env)
      (if (eq? env the-empty-environment)
	  (error "Unbound variable" var)
	  (let ((binding (assoc var (frame-bindigns (first-frame env)))))
	    (if binding
		(cdr binding)
		(env-loop (enclosing-environment env))))))
    (cdr (env-loop env)))

  (define (set-variable-value! var val env)
    (define (env-loop env)
      (if (eq? env the-empty-environment)
	  (error "Unbound variable -- SET!" var)
	  (let ((binding (assoc var (frame-bindigns (first-frame env)))))
	    (if binding
		(set-car! binding val)
		(env-loop (enclosing-environment env))))))
    (cdr (env-loop env)))

  (define (define-variable! var val env)
    (let* ((frame (first-frame))
	   (binding (assoc var (frame-bindigns frame))))
      (if binding
	  (set-car! binding val)
	  (add-binding-to-frame! var val frame))))


)
