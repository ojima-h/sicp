(use gauche.test)
(test-start "Arithmetic Packages")

(require "package")
(require "main")

(test-section "polynomial package")

(install-polynomial-package)

(define zero-term (make-polynomial 'x `((10 ,(n 0)) (3 ,(n 0)))))
(test "is zero"
      #t (lambda () (=zero? zero-term)))

(define zero-term-poly (make-polynomial 'y `((10 z) (3 ,(n 0)))))
(test "is zero"
      #t (lambda () (=zero? zero-term)))

(define poly (make-polynomial 'x `((10 ,(n -2)) (2 ,(make-polynomial 'y `((3 ,(n 2)) (1 ,(n 1))))))))
(test* "negation"
       (make-polynomial 'x `((10 ,(n 2)) (2 ,(make-polynomial 'y `((3 ,(n -2)) (1 ,(n -1)))))))
       (negation poly))

(test-end :exit-on-failure #f)
