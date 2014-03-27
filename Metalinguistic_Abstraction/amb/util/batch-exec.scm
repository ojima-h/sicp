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

(define (batch-exec terms)
  (driver-loop (extend-read-func-with-list read-func-terminal terms)))
(define (exec-and-loop terms)
  (driver-loop (extend-read-func-with-list read-func-interactive terms)))
