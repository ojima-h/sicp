;; PHASE 1
(controller
  (assign g (const 1.0))

  test-b
  (test (op good-enough?) (reg g))
  (branch (label sqrt-done))
  (assign t (op improve) (reg g))
  (assign g (op sqrt-iter) (reg t))
  (goto (label test-b))

  sqrt-done)

;; PHASE 2
(controller
  (assign g (const 1.0))

  test-b

  (assign t (op square) (reg g))
  (assign t (op -) (reg t) (reg x))
  (assign t (op abs) (reg t))
  (test (op <) (reg t) (const 0.001))

  (branch (label sqrt-done))
  (assign t (op improve) (reg g))
  (assign g (op sqrt-iter) (reg t))
  (goto (label test-b))

  sqrt-done)

;; PHASE 3
(controller
  (assign g (const 1.0))

  test-b

  (assign t (op square) (reg g))
  (assign t (op -) (reg t) (reg x))
  (assign t (op abs) (reg t))
  (test (op <) (reg t) (const 0.001))

  (branch (label sqrt-done))

  (assign t (op /) (reg x) (reg g))
  (assign t (op average) (reg t) (reg g))
  (assign g (op sqrt-iter) (reg t))
  (goto (label test-b))

  sqrt-done)
