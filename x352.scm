(load "./x351.scm")

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials (cons-stream 1 (mul-streams integers factorials)))

; 3.55
(define (partial-sums s)
  (add-streams s
	       (cons-stream 0 (partial-sums s))))

; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S
  (cons-stream 1
	       (merge (merge (scale-stream S 2)
			     (scale-stream S 3))
		      (scale-stream S 5))))

; 3.57
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
;; fibs
;; -> (0 . <1>(1 . <2>(add (cdr fibs) fibs)))
;; => (0 . [1](1 . <2>(add (cdr fibs) fibs)))
;; => (0 . [1](1 . [2](add [1](1 . [2](add (cdr fibs) fibs))
;;  			(0 . [1](1 . [2](add (cdr fibs) fibs))))))
;; -> (0 . [1](1 . [2](1 . <3>(add [2](add (cdr fibs) fibs)
;;  				(1 . [2](add (cdr fibs) fibs))))
;; => ...
;;  
;; steram-cdr 1回呼ぶごとに add が1回実行される。


; 3.58

; (expand num den radix) = num / den を <radix>進数展開する

; (expand num den radix) = (a1 a2 a3 ...)
;   <=> num / den = a1 * radix^(-1) + a2 * radix^(-2) + a3 * radix^(-3) + ...
;   <=> (num * radix - a1 * den) / den = a2 * radix^(-1) + a3 * radix^(-2) + ...
;       ^^^^^^^^^^^^^^^^^^^^^^^^
;           = num * radix % den

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;;; test ;;;
;; (stream-take (expand 1 7 10) 10)
;; (/ 1 7.)

; 3.59

; (a)

(define (div-streams s1 s2) (stream-map / s1 s2))
(define (integrate-series series)
  (div-streams series integers))

;;; test ;;;
;;(stream-take (integrate-series ones) 10)

; (b)

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;; test ;;;
;; (stream-take exp-series 10)
;; (stream-take sine-series 10)
;; (stream-take cosine-series 10)

; 3.60

;;  (a1 + a2 * x + a3 * x^2 + ...)
;;     * (b1 + b2 * x + b3 * x^2 + ...)
;;  = a1 * (b1 + b2 * x + b3 * x^2 + ...)
;;      + x * (a2 + a3 * x + ...) * (b1 + b2 * x + b3 * x^2 + ...)
;;  = a1 * b1 + x * { a1 * (b2 + b3 * x + ...)
;;                    + (a2 + a3 * x + ...) * (b1 + b2 * x + b3 * x^2 + ...) }

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
			    (mul-series (stream-cdr s1) s2))))

;;; test ;;;
;; (stream-take (mul-series integers integers) 10)
;; (stream-take (add-streams (mul-series sine-series sine-series)
;;  			  (mul-series cosine-series cosine-series)) 10)

; 3.61

(define (invert-unit-series series)
  (cons-stream 1
	       (scale-stream
		(mul-series (stream-cdr series)
			    (invert-unit-series series)) -1)))

;;; test ;;;
;; (stream-take (mul-series cosine-series (invert-unit-series cosine-series)) 10)

; 3.62

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))
