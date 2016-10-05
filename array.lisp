;; リストではなく配列を使う主な利点は、どの場所にも定数時間でアクセス可能なこと。
;; リストで出来ることはほぼ全て配列でも実現可能。
;; ただ、特定の要素にアクセスするのに、配列はリストよりもずっと速い。違いは性能にある。

(make-array 3)
;; #(0 0 0)


;; 先頭のハッシュ文字(#)で、配列はリストと区別される

(defparameter x (make-array 3))
;; #(0 0 0)

(aref x 1) ; 1番目の要素を取得
;; 0

(setf (aref x 1) 'foo)

(print x)
;; #(0 FOO 0)


(setf foo (list 'a 'b 'c))
;; (A B C)

(second foo)
;; B

(setf (second foo) 'z)
(princ foo)
;; A Z C


;; nthが実用的に役に立つのは、リストの最初の方にアクセスする場合のみ
;; (nth 1000 x)で1000番目の要素にアクセスするのはとても遅い。
;; => コンスセルの連鎖を辿るため。
(nth 1 '(foo bar baz))

;; 配列に(aref x 1000)とすると、1000番目の要素に直接アクセス可能。
(aref (make-array 1001) 1000)


;; initial-contentsで初期値を設定可能。
(make-array 5 :initial-contents '(1 2 3 4 5))
;; #(1 2 3 4 5)
