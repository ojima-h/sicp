(define-module syntax
  (export installed-syntax?
	  install-syntax
	  eval-syntax)

  (define dispatch-table (make-hash-table 'equal?))
  (define (get op type) (hash-table-get dispatch-table (list op type)))
  (define (put op type proc) (hash-table-put! dispatch-table (list op type) proc))
  (define (op-installed? op type) (hash-table-exists? dispatch-table (list op type)))

  (define (installed-syntax? keyword) (op-installed? 'eval keyword))
  (define (install-syntax keyword evaluator :optional builder)
    (put 'eval keyword evaluator)
    (unless (undefined? builder) (put 'build keyword builder))
    'ok)

  (define (eval-syntax keyword exp env)
    ((get 'eval keyword) exp env))
  (define (build-syntax keyword . arguments)
    (apply (get 'build keyword) arguments))
)
