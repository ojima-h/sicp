(define (let? exp) (tagged-list? exp 'let))

(define (let-named? exp) (variable? (cadr exp)))
(define (let-name exp) (cadr exp))

(define (let-bindings exp)
  (if (let-named? exp) (caddr exp) (cadr exp)))
(define (let-body exp)
  (if (let-named? exp) (cdddr exp) (cddr exp)))

(define (let-binding-var binding) (car binding))
(define (let-binding-val binding) (cadr binding))

;; (define (make-let bindings body)
;;   (cons 'let (cons bindings body)))
(define (make-named-let name bindings body)
  (cons 'let (cons name (cons bindings body))))

(define (let-named->combination exp)
  (let ((name     (let-name     exp))
	(bindings (let-bindings exp))
	(body     (let-body     exp)))
    (let ((vars (map let-binding-var bindings))
	  (vals (map let-binding-val bindings)))
      (make-let '()
		(list
		 (list 'define name (make-lambda vars body))
		 (cons name vals))))))

(define (let-unnamed->combination exp)
  (let ((bindings (let-bindings exp))
	(body    (let-body     exp)))
    (let ((vars (map let-binding-var bindings))
	  (vals (map let-binding-val bindings)))
      (cons (make-lambda vars body)
	    vals))))

(define (let->combination exp)
  (if (let-named? exp)
      (let-named->combination exp)
      (let-unnamed->combination exp)))

(put 'eval 'let  (lambda (exp env)
		   (eval (let->combination exp) env)))
  ;; test
(let ()
  (define let-named-exp
    '(let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

  (let->combination let-named-exp))

