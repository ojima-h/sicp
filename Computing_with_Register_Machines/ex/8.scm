(load "ch5-regsim.scm")

;; SAMPLE
(define gcd-machine
  (make-machine
   '(a)
   '()
   '(start
      (goto (label here))
      here
      (assign a (const 3))
      (goto (label there))
      here
      (assign a (const 4))
      (goto (label there))
      there)))

#?=(start gcd-machine)

#?=(get-register-contents gcd-machine 'a)

#?="IMPROVE"

(set! extract-labels
  (lambda (text receive)
    (if (null? text)
        (receive '() '())
        (extract-labels (cdr text)
         (lambda (insts labels)
           (let ((next-inst (car text)))
             (if (symbol? next-inst)
               (if (assoc next-inst labels)
                 (error "Label is already defined -- ASSEMBLE" next-inst)
                 (receive insts
                          (cons (make-label-entry next-inst
                                                  insts)
                                labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels))))))))
(define gcd-machine
  (make-machine
   '(a)
   '()
   '(start
      (goto (label here))
      here
      (assign a (const 3))
      (goto (label there))
      here
      (assign a (const 4))
      (goto (label there))
      there)))
