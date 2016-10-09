;; ハッシュテーブル
;; alistに似ている。
;; キーに結び付けられた値を取り出す。alistよりも速い。

(make-hash-table)


;; alistと同じようにハッシュテーブルは要素をキーと値の組み合わせで格納する
;; キーを使って要素を取り出すのはgethash
(defparameter x (make-hash-table))
(gethash 'yup x)
;; NIL
;; NIL

;; gethashは複数の値を返す。
;; 最初に返される値: ハッシュテーブルに保存されていた値
;; 2番目の値: キーがテーブルの中に存在したかどうかを示す


(defparameter x (make-hash-table))
(setf (gethash 'yup x) '25)
(gethash 'yup x)
;; 25, T


(defparameter *drink-order* (make-hash-table))
(setf (gethash 'bill *drink-order*) 'double-espresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(setf (gethash 'john *drink-order*) 'medium-latte)

(gethash 'lisa *drink-order*)
;; SMALL-DRIP-COFFEE
;; T


;; remhash: 値の削除
(defparameter *table1* (make-hash-table))
(setf (gethash 'yeah *table1*) 'yeah)
(gethash 'yeah *table1*)
;; YEAH, T
(remhash 'yeah *table1*)
;; T
(gethash 'yeah *table1*)
;; NIL, NIL
(remhash 'yeah *table1*)
;; NIL
