;;; 文字列ストリーム
;;; デバッグに利用したり、あるいは複雑な文字列を効率よく作成するのに便利

;;; make-string-output-stream
(defparameter foo (make-string-output-stream))
(princ "This will go into foo." foo)
(print "This will also go into foo." foo)
(get-output-stream-string foo)
;; "This will go into foo.
;; \"This will also go into foo.\" "


;;; with-output-to-string
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
;; "the sum of 5 and 2 is 7"

;;; with-output-to-stringマクロはコンソールやREPL、あるいは他のストリームへ向かうはずだった出力を横取りして、それを文字列へと格納して返す。
;;; with-output-to-stringの本体の実行が終わると、文字列ストリームに蓄積された出力が文字列として返される。
;;; 長く複雑な文字列を組み立てるときにも使える。本体の中で文字列の部分部分をprintしていって、最後に集められた出力を文字列として得る。
;;; 文字列の断片をconcatenateするよりも読みやすく、効率も良いコードとなる。

