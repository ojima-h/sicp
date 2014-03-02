(define (unbinding? exp)
  (tagged-list? exp 'make-unbound!))
(define (unbinding-variable exp) (cadr exp))


(define (eval-unbinding exp env)
  (unset-variable-value! (unbinding-variable exp)
			 env)
  'ok)

(define (unset-variable-value! var env)
  (traverse-environment var env
			(lambda (vals) (pop! vals))
			(lambda (frame) '()))) ; do nothing


(put 'eval 'make-unbound! eval-unbinding)


;; 現在のフレームに該当する変数が見つからなかった場合、更にフレームをたどることはしない。
;; procedure 内部で make-unbound! が呼ばれたときにクロージャの外まで影響を及ぼすのは望ましくない。
