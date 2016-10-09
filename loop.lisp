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


;;; 値を集めてリストにする
(loop for i
     in '(2 3 4 5 6 7)
   collect (* i i))
;; (4 9 16 25 36 49)


;;; 複数のfor節を使う
;; どちらの変数も同時に0から9まで変化する
(loop for x below 10
     for y below 10
   collect (+ x y))
;; (0 2 4 6 8 10 12 14 16 18)


;;; デカルト積
(loop for x below 10
     collect (loop for y below 10
                collect (+ x y)))
;; ((0 1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10 11)
;;  (3 4 5 6 7 8 9 10 11 12) (4 5 6 7 8 9 10 11 12 13)
;;  (5 6 7 8 9 10 11 12 13 14) (6 7 8 9 10 11 12 13 14 15)
;;  (7 8 9 10 11 12 13 14 15 16) (8 9 10 11 12 13 14 15 16 17)
;;  (9 10 11 12 13 14 15 16 17 18))


(loop for i
   from 0
   for day
   in '(monday thesday wednesday thursday friday saturday sunday)
   collect (cons i day))
;; ((0 . MONDAY) (1 . THESDAY) (2 . WEDNESDAY) (3 . THURSDAY) (4 . FRIDAY)
;;  (5 . SATURDAY) (6 . SUNDAY))
