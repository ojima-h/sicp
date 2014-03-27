(load "../amb/main.scm")

(exec-and-loop
 (define (distinct? lst)
   (if (null? lst)
       true
       (let ((x (car lst)))
	 (and (every (lambda (y) (not (= x y))) (cdr lst))
	      (distinct? (cdr lst))))))
 (define (liars-puzzle)
   (let ((betty (amb 3 1))
	 (ethel (amb 1 5))
	 (joan  (amb 2 3))
	 (kitty (amb 2))
	 (mary  (amb 4)))
     (require (distinct? (list betty ethel joan kitty mary)))
     (list
      (list 'betty  betty)
      (list 'ethel  ethel)
      (list 'joan   joan )
      (list 'kitty  kitty)
      (list 'mary   mary ))))
 (liars-puzzle))
