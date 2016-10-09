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






