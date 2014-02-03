(require "package")

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))


  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
     (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))


  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))


  ;; Exercise 2.87.
  ;; Install =zero? for polynomials in the generic arithmetic package. This will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials.
  (define (=zero?-poly poly)
    (let ((term-list (term-list poly)))
      (or (empty-termlist? term-list)
          (and  (=zero? (coeff (first-term term-list)))
                (=zero?-poly (rest-terms term-list))))))

  ;; Exercise 2.88.
  ;; Extend the polynomial system to include subtraction of polynomials. (Hint: You may find it helpful to define a generic negation operation.)
  (define (negation-terms term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (let ((term (first-term term-list)))
          (adjoin-term (make-term (order term) (negation (coeff term)))
                       (negation-terms (rest-terms term-list))))))
  (define (negation-poly poly)
    (make-poly (variable poly)
               (negation-terms (term-list poly))))
  (define (sub-terms L1 L2) (add-terms L1 (negation-terms L2)))
  (define (sub-poly p1 p2) (add-poly p1 (negation-poly p2)))


  ;; Exercise 2.91.
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (sub-terms L1
                                  (mul-term-by-all-terms (make-term new-o new-c) L2))))
                  (let ((result (div-terms rest-of-result L2)))
                    (let ((quotient (car result))
                          (residual (cadr result)))
                      (list (adjoin-term (make-term new-o new-c) quotient)
                            residual)))))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((results (div-terms (term-list p1)
                                  (term-list p2))))
          (list (make-poly (variable p1) (car results))
                (make-poly (variable p1) (cadr results))))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  ;; Exercise 2.94
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  (define (gcd-terms_ a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (gcd-poly p q)
    (let ((a (term-list p))
          (b (term-list q)))
      (make-poly (variable p) (gcd-terms a b))))

  ;; Exercise 2.96.
  ;;   a.    Implement the procedure pseudoremainder-terms, which is just like remainder-terms except that it multiplies the dividend by the integerizing factor described above before calling div-terms. Modify gcd-terms to use pseudoremainder-terms, and verify that greatest-common-divisor now produces an answer with integer coefficients on the example in exercise 2.95.

  ;;   b.    The GCD now has integer coefficients, but they are larger than those of P1. Modify gcd-terms so that it removes common factors from the coefficients of the answer by dividing all the coefficients by their (integer) greatest common divisor.

  (define (pseudoremainder-terms a b)
    (let ((c  (coeff (first-term b)))
          (o1 (order (first-term a)))
          (o2 (order (first-term b))))
      (if (< o1 o2)
          a
          (let ((a1 (mul-term-by-all-terms (make-term 0 (pow c (+ 1 o1 (- o2)))) a)))
            (remainder-terms a1 b)))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (let* ((gcd_tms (gcd-terms b (pseudoremainder-terms a b)))
               (coeffs (map* coeff gcd_tms first-term rest-terms empty-termlist?))
               (factor (apply gcd (map contents coeffs))))
          (mul-term-by-all-terms (make-term 0 (n (/ factor))) gcd_tms))))


;; Exercise 2.97.
;;   a. Implement this algorithm as a procedure reduce-terms that takes two term lists n and d as arguments and returns a list nn, dd, which are n and d reduced to lowest terms via the algorithm given above. Also write a procedure reduce-poly, analogous to add-poly, that checks to see if the two polys have the same variable. If so, reduce-poly strips off the variable and passes the problem to reduce-terms, then reattaches the variable to the two term lists supplied by reduce-terms.
  (define (reduce-terms n d)
    (let* ((gcd-tms (gcd-terms n d))

           (c (coeff (first-term gcd-tms)))
           (o1 (max (order (first-term n)) (order (first-term d))))
           (o2 (order (first-term gcd-tms)))
           (factor (pow c (+ 1 o1 (- o2))))

           (n1 (mul-term-by-all-terms (make-term 0 factor) n))
           (d1 (mul-term-by-all-terms (make-term 0 factor) d))

           (n2 (car (div-terms n1 gcd-tms))) 
           (d2 (car (div-terms d1 gcd-tms))))

      (let* ((n-coeffs (map* coeff n2 first-term rest-terms empty-termlist?))
             (d-coeffs (map* coeff d2 first-term rest-terms empty-termlist?))
             (coeffs (map contents (append n-coeffs d-coeffs)))
             (g (apply gcd coeffs)))
        (list (mul-term-by-all-terms (make-term 0 (make-scheme-number (/ g))) n2)
              (mul-term-by-all-terms (make-term 0 (make-scheme-number (/ g))) d2)))))
        
  ;; test
  ;; (reduce-terms
  ;;  '(sparse-term-list (5 (scheme-number . 2)) (3 (scheme-number . 2)) (2 (scheme-number . 2)) (0 (scheme-number . 2)))
  ;;  '(sparse-term-list (4 (scheme-number . 1)) (2 (scheme-number . 2)) (0 (scheme-number . 1))))

  (define (reduce-poly pn pd)
    (let ((v (variable pn)))
      (map (lambda (p) (make-poly v p))
           (reduce-terms (term-list pn) (term-list pd)))))

  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))

  (put '=zero? '(polynomial) =zero?-poly)
  (put 'negation '(polynomial)
       (lambda (p) (tag (negation-poly p))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))

  (put 'reduce '(polynomial polynomial)
       (lambda (pn pd) (map tag (reduce-poly pn pd))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


;; Exercise 2.89.
;; Define procedures that implement the term-list representation described above as appropriate for dense polynomials.

;; term-list = (order . (c_n ... c_1 c_0))
;; term      = (order . c_i)

(begin
  (define (make-sized-list size list) (cons size list))
  (define (sized-length sized-list) (car sized-list))
  (define (sized-body   sized-list) (cdr sized-list))
  (define sized-null '(0 . ()))
  (define (sized-null? sized-list) (eq? 0 (sized-length sized-list)))
  (define (sized-cons x sized-list)
    (make-sized-list (+ (sized-length sized-list) 1)
                     (cons x (sized-body sized-list))))
  (define (sized-car sized-list) (car (sized-body sized-list)))
  (define (sized-cdr sized-list)
    (make-sized-list (- (sized-length sized-list) 1)
                     (cdr (sized-body sized-list)))))

(define (install-dense-term-list)
  ;; representation of terms and term lists
  (define (dense-adjoin-term term term-list)
    (cond ((=zero? (dense-coeff term)) term-list)
          ((= (- (dense-order term) (dense-term-list-order term-list)) 1)
           (sized-cons (dense-coeff term) term-list))
          (else (dense-adjoin-term term (sized-cons (n 0) term-list)))))
  (define (dense-the-empty-termlist) sized-null)
  (define (dense-term-list-order term-list) (- (sized-length term-list) 1))

  (define (dense-first-term term-list)
    (dense-make-term (dense-term-list-order term-list)
                     (sized-car term-list)))
  (define (dense-rest-terms term-list)

    (if (dense-empty-termlist? term-list)
        (dense-the-empty-termlist)
        (let ((rest-terms (sized-cdr term-list)))
          (if (or (sized-null? rest-terms)
                  (not (=zero? (dense-coeff (dense-first-term rest-terms)))))
              rest-terms
              (dense-rest-terms rest-terms)))))

  (define (dense-empty-termlist? term-list) (sized-null? term-list))

  (define (dense-make-term order coeff) (list order coeff))
  (define (dense-order term) (car term))
  (define (dense-coeff term) (cadr term))

  (define (tag p) (attach-tag 'dense-term-list p))

  (put 'adjoin-term '(dense-term-list)
       (lambda (term-list)
         (lambda (term) (tag (dense-adjoin-term term term-list)))))
  (put 'the-empty-termlist 'dense-term-list
       (lambda () (tag (dense-the-empty-termlist))))
  (put 'first-term '(dense-term-list)
       (lambda (term-list) (dense-first-term term-list)))
  (put 'rest-terms '(dense-term-list)
       (lambda (term-list) (tag (dense-rest-terms term-list))))
  (put 'empty-termlist? '(dense-term-list)
       (lambda (term-list) (dense-empty-termlist? term-list)))
  (put 'make 'dense-term-list
       (lambda (term-list) (tag term-list)))
  'done)

(define (make-term-list-by-dense-form terms)
  ((get 'make 'dense-term-list) terms))
(define (the-empty-termlist)
  ((get 'the-empty-termlist 'dense-term-list)))

(begin
  (define (adjoin-term term term-list)
    ((apply-generic 'adjoin-term term-list) term))
  (define (first-term term-list) (apply-generic 'first-term term-list))
  (define (rest-terms term-list) (apply-generic 'rest-terms term-list))
  (define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list)))
  

;; test
(install-dense-term-list)
(define t (make-term-list-by-dense-form (cons 4 (map n '(3 1 5 2)))))
(define s (adjoin-term (list 6 (n 1)) t))
(first-term s)
(rest-terms s)
(empty-termlist? s)


;; Exercise 2.90.
;; Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term-list representations in our system. The situation is analogous to the complex-number example of section 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.
(define (install-sparse-term-list)
  (define (sparse-adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (sparse-the-empty-termlist) '())
  (define (sparse-first-term term-list) (car term-list))
  (define (sparse-rest-terms term-list) (cdr term-list))
  (define (sparse-empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (tag p) (attach-tag 'sparse-term-list p))

  (put 'adjoin-term '(sparse-term-list)
       (lambda (term-list)
         (lambda (term) (tag (sparse-adjoin-term term term-list)))))
  (put 'the-empty-termlist 'sparse-term-list
       (lambda () (tag (sparse-the-empty-termlist))))
  (put 'first-term '(sparse-term-list)
       (lambda (term-list) (sparse-first-term term-list)))
  (put 'rest-terms '(sparse-term-list)
       (lambda (term-list) (tag (sparse-rest-terms term-list))))
  (put 'empty-termlist? '(sparse-term-list)
       (lambda (term-list) (sparse-empty-termlist? term-list)))
  (put 'make 'sparse-term-list
       (lambda (term-list) (tag term-list)))
  'done)

(define (make-term-list-by-sparse-form terms)
    ((get 'make 'sparse-term-list) terms))

;; test
(install-sparse-term-list)

(define t (make-term-list-by-sparse-form '((10 1) (2 1))))
(define s (adjoin-term (list 12 (n 1)) t))
(first-term s)
(rest-terms (rest-terms s))
(empty-termlist? s)


;; test
(install-polynomial-package)
(define s11
  (make-polynomial 'y
                   (make-term-list-by-sparse-form `((3 ,(n 2)) (1 ,(n 1))))))
(define s12
  (make-polynomial 'y
                   (make-term-list-by-dense-form (cons 3 (map n '(1 0 1))))))
(define p1
  (make-polynomial 'x
                   (make-term-list-by-sparse-form `((10 ,s11) (2 ,s12)))))

(define s2
  (make-polynomial 'y
                   (make-term-list-by-dense-form (cons 2 (map n '(1 1))))))
(define p2
  (make-polynomial 'x
                   (make-term-list-by-dense-form (cons 3 (list s11 s2 s12)))))


(mul p1 p2)

(define p (make-polynomial 'x
                   (make-term-list-by-dense-form (cons 2 (map n '(1 1))))))
(mul p p)



;; test for Exercise 2.91.
(define p (make-polynomial 'x
                   (make-term-list-by-dense-form (cons 3 (map n '(1 0 -1))))))
(define q (make-polynomial 'x
                   (make-term-list-by-sparse-form `((5 ,(n 1)) (0 ,(n -1))))))
(apply-generic 'div q p)



;; Exercise 2.93.
;; Modify the rational-arithmetic package to use generic operations, but change make-rat so that it does not attempt to reduce fractions to lowest terms. 
(begin
  (define (install-rational-poly-package)
    ;; internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d) (cons n d))
    (define (add-rat x y)
      (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
                (mul (denom x) (denom y))))
    (define (sub-rat x y)
      (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
                (mul (denom x) (denom y))))
    (define (mul-rat x y)
      (make-rat (mul (numer x) (numer y))
                (mul (denom x) (denom y))))
    (define (div-rat x y)
      (make-rat (mul (numer x) (denom y))
                (mul (denom x) (numer y))))
    ;; interface to rest of the system
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))

    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    'done)
  (define (make-rational-poly n d)
    ((get 'make 'rational) n d)))

;; test
(install-rational-poly-package)
(define p1 (make-polynomial 'x (make-term-list-by-sparse-form `((2 ,(n 1)) (0 ,(n 1)))))) 
(define p2 (make-polynomial 'x (make-term-list-by-sparse-form `((3 ,(n 1)) (0 ,(n 1)))))) 
(define rf (make-rational-poly p2 p1))


;; Exercise 2.94.  Using div-terms, implement the procedure remainder-terms and use this to define gcd-terms as above. Now write a procedure gcd-poly that computes the polynomial GCD of two polys. (The procedure should signal an error if the two polys are not in the same variable.) Install in the system a generic operation greatest-common-divisor that reduces to gcd-poly for polynomials and to ordinary gcd for ordinary numbers. As a test, try

(define p1 (make-polynomial 'x (make-term-list-by-sparse-form `((4 ,(n 1)) (3 ,(n -1)) (2 ,(n -2)) (1 ,(n 2))))))
(define p2 (make-polynomial 'x (make-term-list-by-sparse-form `((3 ,(n 1)) (1 ,(n -1))))))
(greatest-common-divisor p1 p2)


;; Exercise 2.95.

(define p1 (make-polynomial 'x (make-term-list-by-sparse-form `((2 ,(n  1)) (1 ,(n -2)) (0 ,(n 1))))))
(define p2 (make-polynomial 'x (make-term-list-by-sparse-form `((2 ,(n 11))             (0 ,(n 7))))))
(define p3 (make-polynomial 'x (make-term-list-by-sparse-form `(            (1 ,(n 13)) (0 ,(n 75))))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)



;; Exercise 2.97.
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))
(put 'reduce '(scheme-number scheme-number)
     (lambda (x y)
       (map (lambda (n) (attach-tag 'scheme-number n))
            (reduce-integers x y))))

(define (reduce x y) (apply-generic 'reduce x y))

(begin
  (define (install-rational-poly-package-mod)
    ;; internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
      (let ((nd (reduce n d)))
        (cons (car nd) (cadr nd))))
    (define (add-rat x y)
      (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
                (mul (denom x) (denom y))))
    (define (sub-rat x y)
      (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
                (mul (denom x) (denom y))))
    (define (mul-rat x y)
      (make-rat (mul (numer x) (numer y))
                (mul (denom x) (denom y))))
    (define (div-rat x y)
      (make-rat (mul (numer x) (denom y))
                (mul (denom x) (numer y))))
    ;; interface to rest of the system
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))

    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    'done)
  (define (make-rational-poly-mod n d)
    ((get 'make 'rational) n d)))

(install-rational-poly-package-mod)

(define p1 (make-polynomial 'x (make-term-list-by-sparse-form `((1 ,(n 1)) (0 ,(n  1))))))
(define p2 (make-polynomial 'x (make-term-list-by-sparse-form `((3 ,(n 1)) (0 ,(n -1))))))
(define p3 (make-polynomial 'x (make-term-list-by-sparse-form `((1 ,(n 1))))))
(define p4 (make-polynomial 'x (make-term-list-by-sparse-form `((2 ,(n 1)) (0 ,(n -1))))))

(define rf1 (make-rational-poly-mod p1 p2))
(define rf2 (make-rational-poly-mod p3 p4))

(add rf1 rf2)

;; (x + 1) / (x^3 - 1) + x / (x^2 - 1)
;;   = { (x + 1) * (x^2 - 1) + x * (x^3 - 1) } / (x^3 - 1)(x^2 - 1)
;;   = { x^3 + x^2 - x - 1 + x^4 - x } / (x^3 - 1)(x^2 - 1)
;;   = { (x - 1)(x^3 + 2 * x^2 + 3 x + 1) } / (x^3 - 1)(x + 1)(x - 1)
;    = (x^3 + 2 * x^2 + 3 x + 1) / (x^4 + x^3 -x -1)
