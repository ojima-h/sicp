(load "amb/main.scm")

(exec-and-loop
 (define (distinct? lst)
   (if (null? lst)
       true
       (let ((x (car lst)))
	 (and (every (lambda (y) (not (= x y))) (cdr lst))
	      (distinct? (cdr lst))))))

 ; moore downing hall barnacle parker  
 ; mary  gabrielle lorna rosalind mellisa

 (define (make-father-info fathers-known)
   (let ((father (list (amb 'mary 'gabrielle 'lorna 'rosalind 'mellisa)
		       (amb 'mary 'gabrielle 'lorna 'rosalind 'mellisa))))
     (require (not (eq? (yackt father) (daughter father))))

     (require (every (lambda (f)
		       (and (not (eq? (yackt    f) (yackt    father)))
			    (not (eq? (daughter f) (daughter father)))))
		     fathers-known))
     father))
 (define (yackt    father) (car father))
 (define (daughter father) (car (cdr father)))

 (define (whos-daughter)
   (let ((moore    (make-father-info '())))
     (require (eq? (daughter moore) 'mary))
     (require (eq? (yackt moore)    'lorna))

     (let ((barnacle (make-father-info (list moore))))
       (require (eq? (yackt barnacle) 'gabrielle))

       (let ((downing  (make-father-info (list moore barnacle))))
         (require (eq? (yackt downing)  'mellisa))
         (require (eq? (yackt downing)  (daughter barnacle)))

         (let ((hall (make-father-info (list moore barnacle downing))))
           (require (eq? (yackt hall)     'rosalind))

           (let ((parker   (make-father-info (list moore barnacle downing hall))))

             (let ((gabrielle-father
                    (find (lambda (father) (eq? (daughter father) 'gabrielle ))
                          (list moore downing hall barnacle parker))))
	       (require (not (null? gabrielle-father)))
               (require (eq? (yackt gabrielle-father) (daughter parker))))


	     (list
	      (list 'moore    moore   )
	      (list 'downing  downing )
	      (list 'hall     hall    )
	      (list 'barnacle barnacle)
	      (list 'parker   parker  ))))))))
 (whos-daughter))
   
;try-again



;; if we are not told that Mary Ann's last name is Moore...

; ((moore (lorna mary)) (downing (mellisa lorna)) (hall (rosalind gabrielle)) (barnacle (gabrielle mellisa)) (parker (mary rosalind)))
; ((moore (lorna gabrielle)) (downing (mellisa rosalind)) (hall (rosalind mary)) (barnacle (gabrielle mellisa)) (parker (mary lorna)))