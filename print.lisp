(print "foo")


;; 改行する, 末尾にスペースがつく
(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))

;; 改行しない
(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))

(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))


(print '3)     ; 3 整数
(print '3.4)   ; 3.4 浮動小数点
(print 'foo)   ; FOO シンボル
(print '"foo") ; "foo" 文字列
(print '#\a)   ; 文字
;; シンボル以外のケースについては'を省略可能



;; princ関数はLispのあらゆるデータ型を取り、出来るだけ人が読みやすい形で表示する
;; 文字列: ダブルクォートで囲まない
;; 文字列は#\を付けずに表示する
(princ '3)     ; 3
(princ '3.4)   ; 3.4
(princ 'foo)   ; FOO
(princ '"foo") ; foo
(princ '#\a)   ; a

(princ #\newline)
(princ #\tab)
(princ #\space)


(progn (princ "This sentence will be interrupted")
       (princ #\newline)
       (princ "by an annoying newline character."))


;; printは出力したものを後でいつでも"読み戻す"ことが可能な形で表示する
;; テキストを出力するのには使えない

;; princはテキストを表示可能だが、一方通行。
;; 一旦princで出力すると、Lispのデータ構造に戻すことができない。


(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))
