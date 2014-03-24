(define nil '())
(define square (lambda (x) (* x x)))

(define (map* proc lst car cdr null?)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (map* proc (cdr lst) car cdr null?))))
