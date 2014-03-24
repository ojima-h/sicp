(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-bindings exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (let*-binding-var binding) (car binding))
(define (let*-binding-val binding) (cadr binding))

(define (last-binding? bindings) (null? (cdr bindings)))

(define (let*->nested-lets exp)
  (define (iter bindings body)
    (if (last-binding? bindings)
	(make-let (list (car bindings)) body)
	(make-let (list (car bindings))
		  (list (iter (cdr bindings) body)))))
  (iter (let*-bindings exp)
	(let*-body exp)))

(put 'eval 'let* (lambda (exp env)
		   (eval (let*->nested-lets exp) env)))
;; test

(let ()
  (define let*-exp
    '(let* ((a 1)
	    (b (+ a 1)))
       (display a)))
       
  (let*->nested-lets let*-exp))
		    
