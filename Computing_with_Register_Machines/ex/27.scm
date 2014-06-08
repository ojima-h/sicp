(load "compat")
(load "load-eceval")

(define the-global-environment (setup-environment))

(start eceval)

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(factorial 1)
(factorial 9)
(factorial 10)

;; maximum depth =  5 * n +  3
;; total pushes  = 32 * n - 16
