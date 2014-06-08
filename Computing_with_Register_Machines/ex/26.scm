(load "compat")
(load "load-eceval")

(define the-global-environment (setup-environment))

(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(factorial 8)
(factorial 9)
(factorial 10)

;; 35 * n + 29
