(load "./x352.scm")

(define (sum . args) (fold + 0 args))
(define (average . args) (/ (apply sum args) (length args)))
(define (sqrt-improve guess x) (average guess (/ x guess)))
(define (square x) (* x x))

;; square root
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


;; pi
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;;;test;;;
;; (display-stream pi-stream 10)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;;;test;;;
;; (display-stream (euler-transform pi-stream) 10)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;;;test;;;
;; (display-stream (accelerated-sequence euler-transform pi-stream) 10)

; 3.63

;; Louis の実装では (sqrt-stream x) が呼び出される度に新しい delay オブジェクトが生成され
;; てしまい、メモ化の意味がなくなるため、非効率である。
;;  
;; (sqrt 2)
;; -> (1.0 . ^(map imp (sqrt 2)))
;; => (1.0 . (map imp (1.0 . ^(map imp (sqrt 2)))))
;; -> (1.0 (imp 1.0) . ^(map imp (map imp (sqrt 2))))
;; => (1.0 (imp 1.0) . (map imp (map imp (1.0 . ^(map imp (sqrt 2))))))
;; -> (1.0 (imp 1.0) (imp (imp 1.0)) . ^(map imp (map imp (map imp (sqrt 2)))))
;;  
;; memo-proc を用いない版と比較すると、実行効率に違いはないが、
;; sqrt-stream の呼び出しのオーバーヘッド分だけちょっと遅い？
;; また、(sqrt-stream x) が呼び出される度に新しい delay オブジェクトが生
;; 成されるため、メモリ使用量が増える。

; 3.64
(define (stream-drop-while pred stream)
  (if (pred (stream-car stream))
      (stream-drop-while pred (stream-cdr stream))
      stream))
(define (stream-zip s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
	       (stream-zip (stream-cdr s1) (stream-cdr s2))))
(define (stream-index pred stream)
  (ref (car (stream-drop-while (.$ pred car)
			       (stream-zip stream integers)))
       1))

(define (stream-limit stream tolerance)
  (define (too-rough? x) (< tolerance (abs (- (car x) (cadr x)))))
  (cadr (stream-car
	 (stream-drop-while too-rough?
			    (stream-zip stream (stream-cdr stream))))))
(define (stream-limit-index stream tolerance)
  (define (too-rough? x) (< tolerance (abs (- (car x) (cadr x)))))
  (stream-index too-rough?
		(stream-zip stream (stream-cdr stream))))
;;;test;;;
;;(stream-limit (euler-transform pi-stream) .0001)
;;(stream-limit-index (euler-transform pi-stream) .0001)

; 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

;;;test;;;
;; (display-stream ln2-stream 10)
;; (display-stream (euler-transform ln2-stream) 10)
;; (display-stream (accelerated-sequence euler-transform ln2-stream) 10)

;; (stream-limit-index ln2-stream .0001) -> 10000 (too slow)
;; (stream-limit-index (euler-transform ln2-stream) .0001)
;; (stream-limit-index (accelerated-sequence euler-transform ln2-stream) .0001)
