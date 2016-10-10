;;; 出力ストリーム
;;; ・出力ストリームかどうかを調べる
;;; ・データをストリームへと送り出す


;; *standard-stream* => 出力ストリーム(REPL)
(output-stream-p *standard-output*)

;; write-char
;; Lispの文字はwrite-charを使って出力ストリームに送り出すことができる。
;; 文字#\xを*standard-output*に送り出す例。
(write-char #\x *standard-output*)
;; xNIL
;; (戻り値はnil)



;;; 入力ストリーム
;;; ・入力ストリームかどうかを調べる。
;;; ・ストリームから要素を一つ取り出す。

;; 入力ストリームかどうかを調べる
(input-stream-p *standard-input*)
;; T

;; 入力ストリームから1文字取り出す
(read-char *standard-input*)
;; #\NewLine

