;; gethash
;; round

(round 2.4)
;; 2          ; 丸めた数
;; 0.4000001  ; 丸めの余り


;; 複数の値を返すにはvaluesを使う
(defun foo ()
  (values 3 7))
;; 3
;; 7


;; デフォルトでは最初の値が使われる
(+ (foo) 5)
;; 8

;; 2番目の値が欲しい場合
;; 1とbをfooの2つの戻り値(3 7)に束縛
(multiple-value-bind (a b) (foo)
  (* a b))
;; 21


