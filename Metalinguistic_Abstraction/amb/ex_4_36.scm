(load "amb/main.scm")

(exec-and-loop
 (define (a-pythagorean-triple)
   (let ((i (an-integer-starting-from 1)))
     (let ((j (an-integer-between 1 i)))
       (let ((k (an-integer-between 1 j)))
	 (require (= (* i i) (+ (* j j) (* k k))))
	 (list k j i)))))
 (a-pythagorean-triple)))
