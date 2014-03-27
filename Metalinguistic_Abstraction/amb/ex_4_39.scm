(load "../amb/main.scm")

(batch-exec
 (define (distinct? lst)
   (if (null? lst)
       true
       (let ((x (car lst)))
	 (and (every (lambda (y) (not (= x y))) (cdr lst))
	      (distinct? (cdr lst))))))

 ;; original
 (define (multiple-dwelling-original)
   (let ((baker (amb 1 2 3 4 5))
	 (cooper (amb 1 2 3 4 5))
	 (fletcher (amb 1 2 3 4 5))
	 (miller (amb 1 2 3 4 5))
	 (smith (amb 1 2 3 4 5)))
     (require
      (distinct? (list baker cooper fletcher miller smith)))
     (require (not (= baker 5)))
     (require (not (= cooper 1)))
     (require (not (= fletcher 5)))
     (require (not (= fletcher 1)))
     (require (> miller cooper))
     (require (not (= (abs (- smith fletcher)) 1)))
     (require (not (= (abs (- fletcher cooper)) 1)))
     (list (list 'baker baker)
	   (list 'cooper cooper)
	   (list 'fletcher fletcher)
	   (list 'miller miller)
	   (list 'smith smith))))

 ;; reordered
 (define (multiple-dwelling-reordered)
   (let ((baker (amb 1 2 3 4 5))
	 (cooper (amb 1 2 3 4 5))
	 (fletcher (amb 1 2 3 4 5))
	 (miller (amb 1 2 3 4 5))
	 (smith (amb 1 2 3 4 5)))
     (require (not (= baker 5)))
     (require (not (= cooper 1)))
     (require (not (= fletcher 5)))
     (require (not (= fletcher 1)))
     (require (not (= (abs (- fletcher cooper)) 1)))
     (require (> miller cooper))
     (require
      (distinct? (list baker cooper fletcher miller smith)))
     (require (not (= (abs (- smith fletcher)) 1)))
     (list (list 'baker baker)
	   (list 'cooper cooper)
	   (list 'fletcher fletcher)
	   (list 'miller miller)
	   (list 'smith smith))))
 
 ;; faster
 (define (multiple-dwelling-faster)
   (let ((baker (amb 1 2 3 4 5)))
     (require (not (= baker 5)))

     (let ((cooper (amb 1 2 3 4 5)))
       (require (not (= cooper 1)))

       (let ((fletcher (amb 1 2 3 4 5)))
	 (require (not (= fletcher 5)))
	 (require (not (= fletcher 1)))
	 (require (not (= (abs (- fletcher cooper)) 1)))

	 (let ((miller (amb 1 2 3 4 5)))
	   (require (> miller cooper))

	   (let ((smith (amb 1 2 3 4 5)))
	     (require
	      (distinct? (list baker cooper fletcher miller smith)))
	     (require (not (= (abs (- smith fletcher)) 1)))

	     (list (list 'baker baker)
		   (list 'cooper cooper)
		   (list 'fletcher fletcher)
		   (list 'miller miller)
		   (list 'smith smith)))))))))
 

(set! backtrack-count 0)
(batch-exec
 (multiple-dwelling-original))
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display ";;;;;; original ;;;;;;")
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display backtrack-count)

(set! backtrack-count 0)
(batch-exec
 (multiple-dwelling-reordered))
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display ";;;;;; reordered ;;;;;")
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display backtrack-count)

(set! backtrack-count 0)
(batch-exec
 (multiple-dwelling-faster))
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display ";;;;;; faster ;;;;;;;;")
(newline) (display ";;;;;;;;;;;;;;;;;;;;;;")
(newline) (display backtrack-count)
