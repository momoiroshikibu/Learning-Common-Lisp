(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (listp a) (listp b)) (append a b))))

(add 3 4)
;; 7

(add '(a b) '(c d))
;; (A B C D)


;; defmethod
;; それぞれの型に特化した複数の関数を同じ名前で定義できる。
;; 自動的に引数の型を調べて、対応する関数本体を呼び出す。
;; type dispatching
(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))

(add 3 4)
;; 7

(add '(a b) '(c d))
;; (A B C D)
