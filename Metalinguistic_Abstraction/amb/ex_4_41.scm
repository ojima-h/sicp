(use srfi-1)
(use util.combinations)

(define (distinct? lst)
  (if (null? lst)
      #t
      (let ((x (car lst)))
	(and (every (lambda (y) (not (= x y))) (cdr lst))
	     (distinct? (cdr lst))))))
(define (condition baker cooper fletcher miller smith)
  (and
   (distinct? (list baker cooper fletcher miller smith))
   (not (= baker 5))
   (not (= cooper 1))
   (not (= fletcher 5))
   (not (= fletcher 1))
   (> miller cooper)
   (not (= (abs (- smith fletcher)) 1))
   (not (= (abs (- fletcher cooper)) 1))))

(display
 (map (lambda (answer)
	(zip '(baker cooper fletcher miller smith) answer))
      (filter
       (lambda (candidate) (apply condition candidate))
       (cartesian-product
	'((1 2 3 4 5)
	  (1 2 3 4 5)
	  (1 2 3 4 5)
	  (1 2 3 4 5)
	  (1 2 3 4 5))))))
