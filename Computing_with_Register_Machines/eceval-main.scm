(add-load-path "./sources" :relative)
(load "compat")
(load "load-eceval.scm")

(define the-global-environment (setup-environment))

(start eceval)
