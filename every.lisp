;; every
;; シーケンス中の要素が全て条件を満たすかを調べる

(every #'oddp '(1 3 5 7))
;; T

(every (lambda (s) (equal #\s s)) "ssssss")
;; T

(every (lambda (s) (equal #\s s)) "ssssss1")
;; NIL

(every #'zerop (make-array 5))
;; T
