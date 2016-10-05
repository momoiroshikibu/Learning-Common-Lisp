;; reduce
;; シーケンスの全ての要素を単一の値へと蒸留するのに使える
;; リスト、配列、文字列に適用可能。

;; リストの要素全てを足し合わせる
(reduce #'+ '(3 4 6 5 2))
;; 20

;; 加算関数は常に1つの中間結果と、シーケンスの次の要素を受け取る。
;; 例外は最初の呼び出しで、この時はまだ中韓結果がないので、シーケンスの最初の要素である3が中間結果として使われる。
;; 最初の関数呼び出しに限り、シーケンスから2つの要素が使われる



;; 縮約関数(reduction function)は2つの引数を取る。
;; 最初の引数:  それまでで最大の偶数
;; 2番目の引数: リストの次の値
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0)
;; 6


;; initial-valueを与えないと、意図通りの結果にならない。
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
              item
              best))
        '(7 4 6 5 2))
;; 7



;; 単一の関数で要素の和を求める
(defun sum (lst)
  (reduce #'+ lst))

(sum '(1 2 3))
;; 6

(sum (make-array 5 :initial-contents '(1 2 3 4 5)))
;; 15

(sum "blablabla")
;; The value  #\bis not of type  NUMBER

