(controller
  (assign c (const 1))
  (assign p (const 1))

  test-b
  (test (op >) (reg c) (reg n))
  (branch (label fib-done))
  (assign p (op *) (reg c) (reg p))
  (assign c (op +) (reg c) (const 1))
  (goto test-b)

  fib-done)
