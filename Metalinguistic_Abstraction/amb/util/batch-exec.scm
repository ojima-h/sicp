(define read-func-terminal
  (let ((called? #f))
    (lambda ()
      (if called?
	  (error "No more input")
	  'exit))))

(define (extend-read-func-with-list read-func lst)
  (let ((rest lst))
    (lambda ()
      (if (null? rest)
	  (read-func)
	  (let ((e (car rest)))
	    (set! rest (cdr rest))
	    (newline) (display "<<< ") (display e)
	    e)))))

(define-syntax batch-exec
  (syntax-rules ()
    ((_ terms ...)
     (driver-loop (extend-read-func-with-list read-func-terminal '(terms ...))))))
(define-syntax exec-and-loop
  (syntax-rules ()
    ((_ terms ...)
     (driver-loop (extend-read-func-with-list read-func-interactive '(terms ...))))))
