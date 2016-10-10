;; with-open-file

;; 出力
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))

;; with-open-fileは出力ストリームを作ってmy-streamという変数に格納している。
;; my-streamはwith-open-fileの本体、つまり最後のカッコが閉じられるまで有効。
;; そしてその間、このストリームに送られたデータはディスク上のdata.txtというファイルに書き出される。
;; printコマンドがmy-streamを出力先として指定しているから。


;; 入力
(with-open-file (my-stream "and.lisp" :direction :input)
  (read my-stream))


;;; printとreadコマンドはCommon Lispのデータ型を読み書き出来る。
;;; これらの関数をFileStreamと一緒に使うのが、ディスク上にデータを保存しておく簡単な方法。


;; 連想リスト(alist)をファイルに書き出して、読み込む例
(let ((animal-noises '((dog . woof)
                      (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))



;; 作ろうとしたファイルが既に存在した場合にどうするか
;; :if-exists
;; エラーとする場合
(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
  (print "my data" my-stream))
;; error opening #P"/Users/kosuke/github/Learning-Common-Lisp/data.txt":
;;   File exists


;; 上書きしてしまう場合
(with-open-file (my-stream "data.txt"
                           :direction :output
                           :if-exists :supersede)
  (print "my-data" my-stream))
