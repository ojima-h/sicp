(load "regsim-traced.scm")

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

     loop
     (test (op =) (reg n) (const 0))
     (branch (label done))
     (assign val (op *) (reg n) (reg val))
     (assign n   (op -) (reg n) (const 1))
     (goto (label loop))

     done
     (perform (op print) (reg val))
     (perform (op print-stack-statistics))

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
     (assign val (const 1))
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
     (perform (op print-stack-statistics))

     (goto (label start)))))

(start fact-rec-machine)
     
1
2
3
4
5

