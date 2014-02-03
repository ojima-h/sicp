(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b)
     (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s :optional length)
  (if (undefined? length)
      (stream-for-each display-line s)
      (for-each display-line (stream-take s length))))
  


(define (display-line x)
  (display x)
  (newline))

(define (stream-take stream n)
  (if (or (stream-null? stream) (= n 0))
      '()
      (cons (stream-car stream)
	    (stream-take (stream-cdr stream) (- n 1)))))

;; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;; test
;; (display-stream 
;;  (stream-map (lambda (x y) (* x y))
;; 	     (cons-stream 1 (cons-stream 2 the-empty-stream))
;; 	     (cons-stream 3 (cons-stream 4 the-empty-stream))))

