(load "regsim.scm")

(define machine-one
  (make-machine
    '(c)
    (list (list 'display display))
    '(
    start
    (perform (op display) (const "one\n"))
    (perform (op display) (label mid))
    (goto (label end))

    mid
    (assign c (const 0))

    end
  )))

(start machine-one)

(set! make-operation-exp
  (lambda (exp machine labels operations)
    (let ((op (lookup-prim (operation-exp-op exp) operations))
          (aprocs
           (map (lambda (e)
                  (if (label-exp? e)
                    (error "Operation on labels is not permitted -- ASSEMBLE")
                    (make-primitive-exp e machine labels)))
                (operation-exp-operands exp))))
      (lambda ()
      (apply op (map (lambda (p) (p)) aprocs))))))

(define machine-two
  (make-machine
    '(c)
    (list (list 'display display))
    '(
    start
    (perform (op display) (const "two\n"))
    (perform (op display) (label mid))
    (goto (label end))

    mid
    (assign c (const 0))

    end
  )))

(start machine-two)
