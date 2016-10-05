;; some
;; シーケンス中に条件を満たす要素が最低1つあるかを調べる

(some #'oddp '(1 2 3))
;; T

(some (lambda (n) (eql n 1)) '(1 2 3))
;; T

(some (lambda (n) (eql n 1)) '(1 2 3))
;; T

(some (lambda (s) (equal s #\s)) "lisp")
;; T

(some (lambda (n) (eql n 0)) (make-array 5))
;; T
