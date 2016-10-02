;; intersection
;; 2つのリストで共有されている要素を求める

(intersection '(1 2) '(1))
;; (1)


(intersection '(a) '(1))
;; NIL


(intersection '(a b c) '(a b c))
;; (C B A)
