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


;; for   ある範囲の値を移動していく変数を宣言する。デフォルトではゼロから始まって整数を順に数えていく。
;; below その値まで来たらforを止めるように指示する。設定したあたい自身は含まれない。
;; sum   与えられた式の値をすべて合計する。
(loop for i
     below 5
   sum i)
;; 10


;;; 始点と終点を指定して数える
(loop for i
   from 5
   to 10
   sum i)
;; 45

;;; リストの要素について繰り返す
(loop for i in '(100 20 3)
   sum i)
;; 123

;;; 繰り返しの中に処理を記述する
(loop for i
   below 5
   do (print i))
;; 0 
;; 1 
;; 2 
;; 3 
;; 4


;;; 特定の条件が満たされた時だけ何かする
(loop for i
   below 10
   when (oddp i)
   sum i)
;; 25


;;; ループを途中で脱出する
(loop for i
   from 0
   do (print i)
   when (= i 5)
   return 'falafel)
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; FALAFEL
