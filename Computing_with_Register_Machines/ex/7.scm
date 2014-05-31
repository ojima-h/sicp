(load "sample/ch5-regsim.scm")

;; SAMPLE
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

#?=(set-register-contents! gcd-machine 'a 206)

#?=(set-register-contents! gcd-machine 'b 40)

#?=(start gcd-machine)

#?=(get-register-contents gcd-machine 'a)

;; RECURSIVE
(define expt-machine
  (make-machine
    '(continue n b val)

    (list (list '= =) (list '- -) (list '* *))

    '(
    (assign continue (label expt-done))

    expt-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))

    (save continue)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-expt))
    (goto (label expt-loop))

    after-expt
    (restore continue)
    (assign val (op *) (reg val) (reg b))
    (goto (reg continue))

    base-case
    (assign val (const 1))
    (goto (reg continue))

    expt-done
  )))

#?=(set-register-contents! expt-machine 'n 5)

#?=(set-register-contents! expt-machine 'b 2)

#?=(start expt-machine)

#?=(get-register-contents expt-machine 'val)

;; ITERATIVE
(define expt-iter-machine
  (make-machine
    '(c p n b)
    (list (list '= =) (list '- -) (list '* *))
    '(
      (assign c (reg n))
      (assign p (const 1))

      expt-iter
      (test (op =) (reg c) (const 0))
      (branch (label expt-done))

      (assign c (op -) (reg c) (const 1))
      (assign p (op *) (reg p) (reg b))
      (goto (label expt-iter))

    expt-done)))

#?=(set-register-contents! expt-machine 'n 5)

#?=(set-register-contents! expt-machine 'b 2)

#?=(start expt-machine)

#?=(get-register-contents expt-machine 'val)
