(define (make-read-func-from-list lst)
  (let ((rest lst))
    (lambda ()
      (if (null? rest)
	  'exit
	  (let ((e (car rest)))
	    (set! rest (cdr rest))
	    e)))))

(define (batch-exec terms)
  (driver-loop (make-read-func-from-list terms)))
