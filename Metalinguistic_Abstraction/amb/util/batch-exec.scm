(define (make-read-func-from-list lst)
  (let ((rest lst))
    (lambda ()
      (if (null? rest)
	  'exit
	  (let ((e (car rest)))
	    (set! rest (cdr rest))
	    (newline) (display "<<< ") (display e)
	    e)))))

(define (batch-exec terms)
  (driver-loop (make-read-func-from-list terms)))
