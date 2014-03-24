(define-module syntax.sequence
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
	  (else (eval (first-exp exps) env)
		(eval-sequence (rest-exps exps) env))))

  (define (begin-actions exp) (cdr exp))
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
	  ((last-exp? seq) (first-exp seq))
	  (else (make-begin seq))))
  (define (make-begin seq) (cons 'begin seq))

  (install-syntax 'begin eval-sequence make-begin)
)
