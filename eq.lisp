;; 1. シンボル同士はeqで比較
;;    シンボル同士の比較にしか使えないが、速い。
;; 2. シンボル同士の比較でなければequalで比較
;;    2つのものが同型である、つまり「同じ見かけ」かどうかを教えてくれる。
;;    equalは基本的なLispデータ型全てに使える。

(defparameter *fruit* 'apple)
;; *FRUIT*
(cond ((eq *fruit* 'apple) 'its-an-apple)
      ((eq *fruit* 'orange) 'its-an-orange))
;; ITS-AN-APPLE


;; equal
(equal 'apple 'apple)
;; T

(equal (list 1 2 3) (list 1 2 3))
;; T

(equal '(1 2 3) (cons 1 (cons 2 (cons 3 ()))))
;; T

(equal 5 5)
;; T

(equal 2.5 2.5)
;; T

(equal #\a #\a)
;; T


;; eqlはeqと違って数値と文字も比較可能
(eql 'foo 'foo)
;; T

(eql 3.4 3.4)
;; T

(eql #\a #\a)
;; T

;; equalpはequalとほぼ同じだが、複雑な比較の場合についてequalよりも少し賢い。
;; 文字列について大文字小文字の使い方が異なっても比較可能、整数と浮動小数点を比較可能

(equalp "Bob smith" "bob smith")
;; T

(equalp 0 0.0)
;; T


;; 残りの比較コマンドはデータ型に特化したもの
;; =            数値
;; string-equal 文字列
;; char-equal   文字
