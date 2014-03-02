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
			(lambda (frame)
			  (unset-variable-value! var val (enclosing-environment env))))) ; do nothing


(put 'eval 'make-unbound! eval-unbinding)


;; 現在のフレームに該当する変数が見つからなかった場合、更にフレームを
;; たどる。
;; クロージャの原則に従う
;; 参照できるのに削除できないのは直感的でない
