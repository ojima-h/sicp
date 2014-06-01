(load "regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(datapath (make-datapath-info)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
	      ((eq? message 'datapath) datapath)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (((machine 'datapath) 'add-instructions!) insts)
		    (update-insts! insts labels machine)
		    insts)))

;; DATAPATH INFO

(define (make-datapath-info)
  (let ((assign-instructions  '())
	(test-instructions    '())
	(branch-instructions  '())
	(goto-instructions    '())
	(save-instructions    '())
	(restore-instructions '())
	(perform-instructions '())

	(registers '())
	(reg-entry-points '())
	(reg-stacked '()))
    (define (uniq-add x lst)
      (if (member x lst) lst (cons x lst)))
    (define (add-instruction! inst)
      (cond ((eq? (car inst) 'assign)
	     (set! assign-instructions (uniq-add inst assign-instructions))

	     (let ((dst (assign-reg-name inst))
		   (src (assign-value-exp inst)))
	       (let ((p (assoc dst registers)))
		 (if p
		     (set-cdr! p (uniq-add src (cdr p)))
		     (set! registers (cons (list dst src) registers))))))
	    
	    ((eq? (car inst) 'test)
	     (set! test-instructions (uniq-add inst test-instructions)))

	    ((eq? (car inst) 'branch)
	     (set! branch-instructions (uniq-add inst branch-instructions)))

	    ((eq? (car inst) 'goto)
	     (set! goto-instructions (uniq-add inst goto-instructions))
	     (let ((dest (goto-dest inst)))
	       (if (register-exp? dest) (uniq-add (register-exp-reg dest) reg-entry-points))))

	    ((eq? (car inst) 'save)
	     (set! save-instructions (uniq-add inst save-instructions))
	     (set! reg-stacked (uniq-add (stack-inst-reg-name inst) reg-stacked)))

	    ((eq? (car inst) 'restore)
	     (set! restore-instructions (uniq-add inst restore-instructions))
	     (set! reg-stacked (uniq-add (stack-inst-reg-name inst) reg-stacked)))

	    ((eq? (car inst) 'perform)
	     (set! perform-instructions (uniq-add inst perform-instructions)))

	    (else (error "Unknown instruction type -- DATAPATH"
			 inst))))

    (define (dispatch message)
      (cond ((eq? message 'add-instructions!)
	     (lambda (insts)
	       (for-each (lambda (inst)
			   (add-instruction! (instruction-text inst)))
			 insts)))
	    ((eq? message 'get-info)
	     (list
	      (list 'instructions
		    (list 'assign  assign-instructions ) 
		    (list 'test    test-instructions   )  
		    (list 'branch  branch-instructions )  
		    (list 'goto    goto-instructions   )  
		    (list 'save    save-instructions   )  
		    (list 'resotre restore-instructions)  
		    (list 'perform perform-instructions))
	      (list 'registers	      registers       )  
	      (list 'reg-entry-points reg-entry-points)  
	      (list 'reg-stacked      reg-stacked     )))))
    dispatch))

(define gcd-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '((assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of n
     (assign n (op -) (reg n) (const 1)); clobber n to n - 1
     (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
     afterfib-n-2                         ; upon return, val contains Fib(n - 2)
     (assign n (reg val))               ; n now contains Fib(n - 2)
     (restore val)                      ; val now contains Fib(n - 1)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))              ; return to caller, answer is in val
     immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
     fib-done)))


;;((instructions
;;  (assign ((assign val (reg n)) (assign val (op +) (reg val) (reg n)) (assign n (reg val)) (assign continue (label afterfib-n-2)) (assign n (op -) (reg n) (const 2)) (assign n (op -) (reg n) (const 1)) (assign continue (label afterfib-n-1)) (assign continue (label fib-done))))
;;  (test ((test (op <) (reg n) (const 2))))
;;  (branch ((branch (label immediate-answer))))
;;  (goto ((goto (reg continue)) (goto (label fib-loop))))
;;  (save ((save val) (save n) (save continue)))
;;  (resotre ((restore val) (restore continue) (restore n)))
;;  (perform ()))
;; 
;; (registers
;;  ((val ((reg n)) ((op +) (reg val) (reg n)))
;;   (n ((reg val)) ((op -) (reg n) (const 2)) ((op -) (reg n) (const 1)))
;;   (continue ((label afterfib-n-2)) ((label afterfib-n-1)) ((label fib-done)))))
;; 
;; (reg-entry-points ())
;; (reg-stacked (val n continue)))
