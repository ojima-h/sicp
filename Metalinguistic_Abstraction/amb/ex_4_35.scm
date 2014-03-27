(load "amb/main")

(exec-and-loop
 (define (an-integer-between from to)
   (if (> from to)
       (amb)
       (amb from (an-integer-between (+ from 1) to))))
 (define (a-pythagorean-triple-between low high)
   (let ((i (an-integer-between low high)))
     (let ((j (an-integer-between i high)))
       (let ((k (an-integer-between j high)))
	 (require (= (+ (* i i) (* j j)) (* k k)))
	 (list i j k)))))
 (a-pythagorean-triple-between 1 14))
