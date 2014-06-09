(load "compat")
(load "load-eceval")

(define the-global-environment (setup-environment))

(start eceval)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))


(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)

;; maximum-depth = 5 * n - 2

;; total-pushes  = S(n)
;; S(n) = S(n - 1) + S(n - 2) + 40
;; S(0) = S(1) = 16

;; [S(n) + 40]/56 = [S(n - 1) + 40]/56 + [S(n - 2) + 40]/56
;; [S(0) + 40] = [S(1) + 40] = 1
;; [S(n) + 40]/56 = Fib(n + 1)

;; S(n) = 56 * Fib(n + 1) - 40
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^
