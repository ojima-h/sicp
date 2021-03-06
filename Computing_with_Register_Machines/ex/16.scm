(load "regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(trace-flag #f))
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
		(if trace-flag
		    (begin (display (instruction-text (car insts)) (current-error-port))
			   (newline (current-error-port))))
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

	      ((eq? message 'trace-on)  (set! trace-flag #t))
	      ((eq? message 'trace-off) (set! trace-flag #f))

              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define fact-iter-machine
  (make-machine
   '(n val)
   (list (list 'read (lambda ()
		       (newline) (display "number> ") (flush)
		       (read)))
	 (list 'print (lambda (val) (newline) (display val)))
	 (list '= =) (list '- -) (list '* *))
   '(
     start
     (assign n (op read))
     (assign val (const 1))
     (perform (op print) (reg n))
     (perform (op initialize-stack))

     loop
     (test (op =) (reg n) (const 0))
     (branch (label done))
     (assign val (op *) (reg n) (reg val))
     (assign n   (op -) (reg n) (const 1))
     (goto (label loop))

     done
     (perform (op print) (reg val))

     (goto (label start)))))

(define fact-rec-machine
  (make-machine
   '(n val continue)
   (list (list 'read (lambda ()
		       (newline) (display "number> ") (flush)
		       (read)))
	 (list 'print (lambda (val) (newline) (display val)))
	 (list '= =) (list '- -) (list '* *))
   '(start
     (assign continue (label fact-done))     ; set up final return address
     (assign n (op read))
     (perform (op print) (reg n))
     (perform (op initialize-stack))

     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller

     fact-done
     (perform (op print) (reg val))

     (goto (label start)))))

(define fact-machine fact-rec-machine)

#?=(fact-machine 'trace-on)
(start fact-machine)
     
1
2
3
4
5
