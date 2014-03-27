(load "../amb/main.scm")

(batch-exec
 (define (distinct? lst)
   (if (null? lst)
       true
       (let ((x (car lst)))
	 (and (every (lambda (y) (not (= x y))) (cdr lst))
	      (distinct? (cdr lst))))))

 (define (multiple-dwelling-faster)
   (let ((fletcher (amb 2 3 4))
	 (cooper (amb 2 3 4 5)))
     (require (not (= (abs (- fletcher cooper)) 1)))

     (let ((miller (an-integer-between 1 cooper)))
       (let ((baker (amb 1 2 3 4))
	     (smith (amb 1 2 3 4 5)))
	 (require (not (= (abs (- smith fletcher)) 1)))
	 (require
	  (distinct? (list baker cooper fletcher miller smith)))

	 (list (list 'baker baker)
	       (list 'cooper cooper)
	       (list 'fletcher fletcher)
	       (list 'miller miller)
	       (list 'smith smith)))))))

(set! backtrack-count 0)
(batch-exec
 (multiple-dwelling-faster))
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display ";;;;;; faster ;;;;;;;;")
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display backtrack-count)
