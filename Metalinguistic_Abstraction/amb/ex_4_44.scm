(load "amb/main.scm")

(define (display-queens queens)
  (define row car)
  (define col cadr)

  (for-each (lambda (c)
	      (display (make-string (- c 1) #\x))
	      (display "o")
	      (display (make-string (- 8 c) #\x))
	      (newline))
	    (map col (sort queens (lambda (a b) (< (row a) (row b)))))))
(define-variable! 'display-queens (list 'primitive display-queens) the-global-environment)


(exec-and-loop
 (define (row queen) (car queen))
 (define (col queen) (car (cdr queen)))
 (define (make-queen row col) (list row col))

 (define (place-queen row-num queens-now-placed)
   (let ((col-num (amb 1 2 3 4 5 6 7 8)))
     (require
      (every (lambda (q)
	       (and (not (eq? row-num (row q)))
		    (not (eq? col-num (col q)))
		    (not (eq? (abs (- row-num (row q)))
			      (abs (- col-num (col q)))))))
	     queens-now-placed))
     (make-queen row-num col-num)))

 (define (eight-queens)
   (display-queens
    (fold (lambda (row-num queens-now-placed)
	    (cons (place-queen row-num queens-now-placed)
		  queens-now-placed))
	  '()
	  '(1 2 3 4 5 6 7 8))))
 (eight-queens))