(define-module syntax.lambda
  (use syntax)

  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))    
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))

  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))

  (install-syntax 'lambda eval-lambda make-lambda)
)
