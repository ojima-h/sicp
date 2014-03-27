;; (add-load-path ".." :relative)

(load "ambeval.scm")
(load "amb/util/batch-exec.scm")

(define the-global-environment (setup-environment))

(batch-exec
 '((define (require p)
     (if (not p) (amb)))
   (define (an-element-of items)
     (require (not (null? items)))
     (amb (car items) (an-element-of (cdr items))))
   (define (an-integer-starting-from n)
     (amb n (an-integer-starting-from (+ n 1))))))

;; (driver-loop)
;; try-againn

(define (sample)
  (batch-exec
   '((define (even-sum-pair list1 list2)
       (let ((a (an-element-of list1))
	     (b (an-element-of list2)))
	 (require (eq? (remainder (+ a b) 2) 0))
	 (list a b)))
     (even-sum-pair '(1 3 5 8) '(20 35 110))
     try-again)))

(define (main argv) (sample))