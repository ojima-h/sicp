(load "regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())

        (the-label-mapping '())
	(breakpoints '()))
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
	      (if (assq insts breakpoints)
		  'break
		  (begin
		    ((instruction-execution-proc (car insts)))
		    (execute))))))
      (define (proceed)  ;; ignore breakpoint for first step
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (set-breakpoint! label offset)
	(let ((insts (lookup-label the-label-mapping label)))
	  (let ((bp (drop insts (- offset 1))))
	    (set! breakpoints (cons (list bp label offset)
				    breakpoints)))))
      (define (cancel-breakpoint! label offset)
	(set! breakpoints
	      (remove (lambda (breakpoint) (equal? (cdr breakpoint) (list label offset))) breakpoints)))

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))

              ((eq? message 'install-label-mapping)
               (lambda (labels) (set! the-label-mapping labels)))
              ((eq? message 'set-breakpoint)    set-breakpoint!)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint!)
              ((eq? message 'proceed-machine) (proceed))

              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-breakpoint    machine label offset) ((machine 'set-breakpoint)    label offset))
(define (cancel-breakpoint machine label offset) ((machine 'cancel-breakpoint) label offset))
(define (proceed-machine   machine) (machine 'proceed-machine))


(define (assemble insts labels machine)
  (update-insts! insts labels machine)
  insts)

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)

    (extract-labels controller-text
      (lambda (insts labels)
	((machine 'install-instruction-sequence)
	 (assemble insts labels machine))

	((machine 'install-label-mapping) labels)))
    machine))

;;;;;;;;;;;;;;;;;

(define fact-machine
  (make-machine
   '(n val)
   (list (list 'read (lambda ()
		       (newline) (display "number> ") (flush)
		       (read)))
	 (list 'print (lambda (val) (newline) (display val)))
	 (list '= =) (list '- -) (list '* *))
   '(
     start
     (assign n (const 5))
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
     (perform (op print) (reg val)))))

(set-breakpoint fact-machine 'loop 2)

#?=(start fact-machine)

(newline) (display (car (get-register-contents fact-machine 'pc)))
(newline) (display (get-register-contents fact-machine 'n))
(newline) (display (get-register-contents fact-machine 'val))

(display "\nPROCEED\n")
#?=(proceed-machine fact-machine)

(newline) (display (car (get-register-contents fact-machine 'pc)))
(newline) (display (get-register-contents fact-machine 'n))
(newline) (display (get-register-contents fact-machine 'val))

(display "\nPROCEED\n")
#?=(proceed-machine fact-machine)

(newline) (display (car (get-register-contents fact-machine 'pc)))
(newline) (display (get-register-contents fact-machine 'n))
(newline) (display (get-register-contents fact-machine 'val))

(display "\nPROCEED without breakpoints\n")
(cancel-breakpoint fact-machine 'loop 2)
#?=(proceed-machine fact-machine)

(newline) (display (car (get-register-contents fact-machine 'pc)))
