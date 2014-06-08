(load "compat")
(load "load-eceval")

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (memo-it obj value)
  (set-car! obj 'evaluated-thunk)
  (set-car! (cdr obj) value)   ; replace exp with its value
  (set-cdr! (cdr obj) '())     ; forget unneeded env
  value)
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(set! eceval-operations
      (append (list
               (list 'memo-it memo-it)
               (list 'delay-it delay-it)
               (list 'thunk? thunk?)
               (list 'thunk-exp thunk-exp)
               (list 'thunk-env thunk-env)
               (list 'evaluated-thunk? evaluated-thunk?)
               (list 'thunk-value thunk-value))
               eceval-operations))

(define eceval-lazy
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))

  (test (op eof?) (reg exp))
  (branch (label done))

  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label force-eval))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;SECTION 5.4.1
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label force-eval))
ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (save proc)

  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply-operand-loop))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply-operand-loop))
  (goto (label unknown-procedure-type))

primitive-apply-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label primitive-apply-last-arg))
  (save env)
  (save unev)
  (assign continue (label primitive-apply-accumulate-arg))
  (goto (label force-eval))
primitive-apply-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label primitive-apply-operand-loop))
primitive-apply-last-arg
  (assign continue (label primitive-apply-accum-last-arg))
  (goto (label force-eval))
primitive-apply-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply-operand-loop
  (assign exp (op first-operand) (reg unev))
  (assign val (op delay-it) (reg exp) (reg env))
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (test (op last-operand?) (reg unev))
  (branch (label compound-apply))
  (assign unev (op rest-operands) (reg unev))
  (goto (label compound-apply-operand-loop))
compound-apply
  (restore proc)
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

force-eval
  (save continue)
  (assign continue (label force-eval-1))
  (goto (label eval-dispatch))
force-eval-1
  (test (op thunk?) (reg val))
  (branch (label force-eval-2))
  (restore continue)
  (goto (reg continue))
force-eval-2
  (assign exp (op thunk-exp) (reg val))
  (assign env (op thunk-env) (reg val))
  (assign continue (label force-eval-1))
  (goto (label eval-dispatch))

;;;SECTION 5.4.2
ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;;SECTION 5.4.3

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label force-eval))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
done
   )))

(set! primitive-procedures
      (append (list
               (list 'modulo modulo))
              primitive-procedures))
(define the-global-environment (setup-environment))

(start eceval-lazy)

(define (cons a b) (lambda (m) (m a b)))
(define (car p) (p (lambda (a b) a)))
(define (cdr p) (p (lambda (a b) b)))

(define (even? n) (= (modulo n 2) 0))

(define ones (cons 1 ones))
(define (add s1 s2) (cons (+ (car s1) (car s2)) (add (cdr s1) (cdr s2))))
(define integers (cons 0 (add ones integers)))
(define (filter p s)
  (if (p (car s))
      (cons (car s) (filter p (cdr s)))
      (filter p (cdr s))))

(define evens (filter even? integers))

(car evens)
(car (cdr evens))
(car (cdr (cdr evens)))
