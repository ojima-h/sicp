(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(let ()
  (define (let? exp) (tagged-list? exp 'let))
  (define (let-bindings exp) (cadr exp))
  (define (let-body exp) (cddr exp))

  (define (let-binding-var binding) (car binding))
  (define (let-binding-val binding) (cadr binding))

  (define (let->combination exp)
    (let ((bindings (let-bindings exp))
	  (body    (let-body     exp)))
      (let ((vars (map let-binding-var bindings))
	    (vals (map let-binding-val bindings)))
	(cons (make-lambda vars body)
	      vals))))

  ;; (put 'eval 'let  (lambda (exp env)
  ;; 		     (eval (let->combination exp) env)))

  ;; test
  (define let-exp
    '(let ((a 1)
	   (b (list 1 2)))
       (display a)
       (map - b)))

  (let->combination let-exp))

