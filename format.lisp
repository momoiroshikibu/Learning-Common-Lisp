;; format
;; 高機能なテキスト表示関数

(format t "Add onion rings for only ~$ dollars more!" 1.5)
;; Add onion rings for only 1.50 dollars more!


;; t => 出力先(nil: 文字列, t: コンソール, その他ストリーム)
;; ~$ が制御シーケンス


;;; 出力先
;;; nil
;;; 出力する代わりに、生成されたテキストを文字列として返す
;;; t
;;; 結果をコンソールに出力する。返り値はnilとなる
;;; stream
;;; データを出力ストリームに書き出す。

(princ (reverse
        (format nil "Add onion rings for only ~$ dollars more!" 1.5)))
;; "!erom srallod 05.1 ylno rof sgnir noino ddA"



;;; 制御文字列
;;; テキストの成形方法
;;; 原則として、制御文字列中のテキストはそのまま出力される。制御シーケンスは出力形式に影響を与える。
;;; 制御シーケンスは常に~で始まる。



;;; 値引数
;;; 整形され表示されるべきデータ。
;;; 制御文字列に従って解釈されて整形される。


;;; 制御シーケンス~s, ~aを使えば、prin1とprincの出力と同じものをformatの出力中に埋め込むことができる。
;;; ~sはprin1とおあん軸、Lispが後から読み込めるような区切り文字が入った形式を出力する。
;;; ~aはprincと同じく、人間が読みやすい形式。
(format t "I am printing ~s in the middle of this sentence." "foo")
;; I am printing "foo" in the middle of this sentence.

(format t "I am printing ~a in the middle of this sentence." "foo")
;; I am printing foo in the middle of this sentence.



;;; ~10a
;;; 値の部分が10文字になるように、fooの右に7個の空白が追加される
(format t "I am printing ~10a within ten spaces of room." "foo")
;; I am printing foo        within ten spaces of room.

;;; 値の左側に空白を含める場合、@を加える
(format t "I am printing ~10@a within ten spaces of room." "foo")
;; I am printing        foo within ten spaces of room.


;;; 制御シーケンスへのパラメータを複数渡す場合は,で区切る
;;; 表示幅を埋める際に、1つではなく3つずつ空白を足すように指示する。空白は全体の表示幅が10文字に等しくなるかそれを超えるまで足される。
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")
;; I am printing foo          within ten (or more) spaces of room.


;;; 少なくとも何文字かの空白(パディング文字数の下限)
(format t "I am printing ~,,4a in the middle of this sentence." "foo")
;; I am printing foo     in the middle of this sentence.

;;; 4番目の制御シーケンスパラメータはパディングに使われる文字
(format t "The word ~,,4,'!a feels very important." "foo")
;; The word foo!!!! feels very important.



;;; 整数の整形

;;; 16進数 => ~x
(format t "The number 1000 in hexadecimal is ~x" 1000)
;; The number 1000 in hexadecimal is 3E8


;;; 2進数
(format t "The number 1000 in binary is ~b" 1000)
;; The number 1000 in binary is 1111101000


;;; 10進数
(format t "The number 1000 in decimal is ~d" 1000)
;; The number 1000 in decimal is 1000

;;; ,による桁のグループ化 => :
(format t "Numbers with commas in them are ~:d times better." 1000000)
;; Numbers with commas in them are 1,000,000 times better.


;;; 表示幅の指定
(format t "I am printing ~10d within ten spaces of room" 10000)
;; I am printing      10000 within ten spaces of room
(format t "I am printing ~10,'xd within ten spaces of room" 10000)
;; I am printing xxxxx10000 within ten spaces of room


;;; 浮動小数点数の整形
;;; ~f制御シーケンス
;;; 指定された表示幅(小数点も含める)に収まるように丸められる
(format t "PI can be estimated as ~4f" 3.141593)
;; PI can be estimated as 3.14

;;; ~fの2番目のパラメータは、小数点以下に表示する桁数。
(format t "PI can be estimated as ~,4f" 3.141593)
;; PI can be estimated as 3.1416

;;; Common Lispにはpiという定数がある
(format t "PI can be estimated as ~,4f" pi)
;; PI can be estimated as 3.1416

;;; ~f制御シーケンスの3番目のパラメータは表示する数を10の指数倍するためのその指数
;; 2を渡すと表示する数は10の2乗倍され、%表示になる
(format t "Percentages are ~,,2f percent better than fractions" 0.77)
;; Percentages are 77.0 percent better than fractions


;;; 価格を表示するときには~fの代わりに~$を使うと良い
(format t "I wish I had ~$ dollars in my bank account." 1000000.2)
;; I wish I had 1000000.20 dollars in my bank account.



;;; 複数行
;; ~% => 常に改行する(terpri相当)
;; ~& => 必要なときだけ改行(fresh-line相当)


(progn (format t "this is on the line ~%")
       (format t "~%this is on another line"))
;; this is on the line 
;; 
;; this is on another line


(progn (format t "this is on one line~&")
       (format t "~&this is on another line"))
;; this is on one line
;; this is on another line


;;; 余分な~%があると空行が増えて出力が醜くなるが、同じ場所に~&を使えばそれを避けられる。
;;; これらのシーケンスは改行数を指定するパラメータを1つ取る。
(format t "this will print ~5%on two lines spread far apart")
;; this will print 
;;
;;
;;
;;
;; on two lines spread far apart
