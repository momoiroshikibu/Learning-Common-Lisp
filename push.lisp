;; push
;; リストを保持している変数の先頭に新しい要素を付け加える

(defparameter *foo* '(1 2 3))
(push 7 *foo*)
*foo*
;; (7 1 2 3)


;; (setf *foo* (cons 7 *foo*)) に置き換えても動作する
