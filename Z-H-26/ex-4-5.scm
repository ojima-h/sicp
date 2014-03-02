(let ()
  (define (cond? exp) (tagged-list? exp 'cond))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

  (define (cond-extended-clause? clause)
    (eq? (cadr clause) '=>))
  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause)
    (if (cond-extended-clause? clause)
	(cddr clause)
	(cdr clause)))

  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

  (define (expand-clauses clauses)
    (if (null? clauses)
	'false                          ; no else clause
	(let ((first (car clauses))
	      (rest (cdr clauses)))
	  (if (cond-else-clause? first)
	      (if (null? rest)
		  (sequence->exp (cond-actions first))
		  (error "ELSE clause isn't last -- COND->IF"
			 clauses))
	      (make-if (cond-predicate first)
		       (if (cond-extended-clause? first)
			   (list (sequence->exp (cond-actions first))
				 (cond-predicate first))
			   (sequence->exp (cond-actions first)))
		       (expand-clauses rest))))))

					; 本当は eval-cond を実装した方がよいと思う。



  ;;test 

  (define clauses
    '(
      ((a b) x
       (y z))
      ((c d) => u v)
      (else p)))

  (expand-clauses clauses)
)
