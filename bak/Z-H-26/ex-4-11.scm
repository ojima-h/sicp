(let ()
  (define (make-frame vars vals) (list (map cons vars vals)))
  (define (frame-bindigns frame) (car frame))
  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons (cons var val) frame)))

  (define (extend-environment vars vals base-env)
    (cons (make-frame vars vals) base-env))

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



(define (setup-environment)
  (let ((initial-env
         (extend-environment (map cons primitive-procedure-names primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true  #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
)
