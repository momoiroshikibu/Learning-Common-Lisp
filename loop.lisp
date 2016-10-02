;; loop
;; 様々な型のデータに対してループすることができる

;; repeatを使って難解ループするかを指定子、
;; collectで1回毎に集めるオブジェクト(ここでは数値1)を指定する
(loop repeat 10
     collect 1)
;; (1 1 1 1 1 1 1 1 1 1)



;; 何回目のループ化をループ内で知りたいことがある
;; nを1から10まで変えながらループする
;; 1回ごとにその時のnを集めてリストにしている
(loop for n from 1 to 10
     collect n)
;; (1 2 3 4 5 6 7 8 9 10)


;; collectの部分には任意のLisp式を書ける
(loop for n from 1 to 10
     collect (+ 100 n))
;; (101 102 103 104 105 106 107 108 109 110)
