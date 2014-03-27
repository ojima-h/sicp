(add-load-path ".." :relative)

(load "ambeval.scm")

(define the-global-environment (setup-environment))

(define (simple-success val next-alternative))
(define (simple-fail))
(define (simple-ambeval exp)
  (ambeval exp the-global-environment simple-success simple-fail))

(simple-ambeval '(define (require p)
		  (if (not p) (amb))))
(simple-ambeval '(define (an-element-of items)
		  (require (not (null? items)))
		  (amb (car items) (an-element-of (cdr items)))))
(simple-ambeval '(define (an-integer-starting-from n)
		   (amb n (an-integer-starting-from (+ n 1)))))

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
  

;; (driver-loop)
;; try-againn

;; SAMPLE

;;; (batch-exec
;;;   '((define (even-sum-pair list1 list2)
;;;       (let ((a (an-element-of list1))
;;; 	    (b (an-element-of list2)))
;;; 	(require (eq? (remainder (+ a b) 2) 0))
;;; 	(list a b)))
;
;;;     (even-sum-pair '(1 3 5 8) '(20 35 110))
;
;;;     try-again))

