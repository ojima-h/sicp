(define (do? exp) (tagged-list? exp 'do))

(define (do-bindings exp) (cadr exp))
(define (do-pred exp) (caddr exp))
(define (do-body exp) (cdddr exp))

(define (do-binding-var binding) (car binding))
(define (do-binding-val binding) (cadr binding))
(define (do-binding-step binding)
  (if (null? (cddr binding)) (do-binding-var binding) (caddr binding)))

(define (do-pred-test test) (car test))
(define (do-pred-exp test) (cdr test))

(define (do->let exp)
  (let ((bindings (do-bindings exp))
	(pred (do-pred exp))
	(body (do-body exp)))
    (make-let '()
	      `((define (__DO_ITER__ ,@(map do-binding-var bindings))
		  ,(make-if (do-pred-test pred)
			    (sequence->exp (do-pred-exp pred))
			    (make-begin
			     `(,@body
			       (__DO_ITER__ ,@(map do-binding-step bindings))))))
		(__DO_ITER__ ,@(map do-binding-val bindings))))))

(put 'eval 'do
     (lambda (exp env)
       (eval (do->let exp) env)))


;; test

(do->let '(do ((vec (make-vector 5))
	       (i 0 (+ i 1)))
              ((= i 5) vec)
	    (vector-set! vec i i)))




(define (while? exp) (tagged-list? exp 'while))

(define (while-pred exp) (cadr exp))
(define (while-body exp) (cddr exp))

(define (while->let exp)
  (let ((pred (while-pred exp))
	(body (while-body exp)))
    (make-let '()
	      `((define (__DO_ITER__)
		  ,(make-if (while-pred exp)
			    (sequence->exp
			     (append (while-body exp)
				     '((__DO_ITER__))))
			    '(quote done)))
		(__DO_ITER__)))))

(put 'eval 'while
     (lambda (exp env)
       (eval (while->let exp) env)))

;; test

(while->let '(while (pair? a)
    (write (pop! a))))




