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

