;; some

(some #'oddp '(1 2 3))
;; T

(some (lambda (n) (eql n 1)) '(1 2 3))
;; T
