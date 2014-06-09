(load "regsim.scm")

(define count-tree-machine
  (make-machine
   '(tree continue val v)
   (list
    (list '+ +)
    (list 'cons cons)
    (list 'car  car)
    (list 'cdr  cdr)
    (list 'set-car! set-car!)
    (list 'set-cdr! set-cdr!)
    (list 'null? null?)
    (list 'pair? pair?))
   '(
     (assign continue (label done))
     (assign val (const 0))

     iter
     (test (op null?) (reg tree))
     (branch (label base-case-0))

     (test (op pair?) (reg tree))
     (branch (label iter-body))
     (goto (label base-case-1))

     iter-body
     (save continue)
     (save tree)

     (assign continue (label after-1))
     (assign tree (op car) (reg tree))
     (goto (label iter))

     after-1
     (restore tree)
     (save val)

     (assign tree (op cdr) (reg tree))
     (assign continue (label after-2))
     (goto (label iter))

     after-2
     (assign v (reg val))

     (restore val)
     (assign val (op +) (reg val) (reg v))

     (restore continue)
     (goto (reg continue))

     base-case-0
     (assign val (const 0))
     (goto (reg continue))

     base-case-1
     (assign val (const 1))
     (goto (reg continue))

     done
     )))

(set-register-contents! count-tree-machine 'tree '(1 (2 3 4) 2 (1 5 2)))
(start count-tree-machine)

(display (get-register-contents count-tree-machine 'val))
(newline)
  
